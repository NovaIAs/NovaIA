```go
package main

import (
	"bufio"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math/rand"
	"net/http"
	"os"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"

	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	"google.golang.org/api/sheets/v4"
)

// Constants for OAuth2 flow.
const (
	redirectURI = "http://localhost:14000/callback"
	scope       = "https://www.googleapis.com/auth/spreadsheets.readonly"
)

// Client is the main data structure of this program.
type Client struct {
	oauthConfig *oauth2.Config
	tokenFile   string
	token       *oauth2.Token
	authURL     string
	client      *http.Client

	// Data fetched from Google Sheets.
	sheetsService *sheets.Service
	sheetID      string
	sheetName    string
	sheetData    [][]interface{}

	// Number of goroutines to be spawned.
	numWorkers int

	// Data channels for inter-goroutine communication.
	workChan    chan int
	resultChan  chan []interface{}
	errorChan   chan error
	resultMutex sync.Mutex
	results     [][]interface{}
}

// worker is a wrapper function for a goroutine. It takes a client and a row number as
// input, reads the corresponding row from the spreadsheet, and sends it to the result
// channel. If an error occurs, it sends the error to the error channel.
func (c *Client) worker(i int) {
	row, err := c.sheetsService.Spreadsheets.Values.Get(c.sheetID, fmt.Sprintf("%s!%d:%d", c.sheetName, i+1, i+1)).Do()
	if err != nil {
		c.errorChan <- fmt.Errorf("worker: could not read row %d: %v", i, err)
		return
	}
	c.resultChan <- row.Values
}

// startWorkers starts numWorkers goroutines, each of which calls worker(i).
func (c *Client) startWorkers() {
	for i := 0; i < c.numWorkers; i++ {
		go c.worker(i)
	}
}

// waitForAllWorkers blocks until all goroutines have finished their work.
func (c *Client) waitForAllWorkers() {
	// Wait for all goroutines to finish.
	for i := 0; i < c.numWorkers; i++ {
		select {
		case <-c.resultChan:
		case err := <-c.errorChan:
			fmt.Println(err)
			os.Exit(1)
		}
	}
}

// fetchSheetData fetches all data from the spreadsheet and stores it in the
// Client's sheetData field.
func (c *Client) fetchSheetData() error {
	// Get the range of cells to read.
	range_ := fmt.Sprintf("%s!A1:%s1000", c.sheetName, c.sheetName)

	// Read the data from the spreadsheet.
	response, err := c.sheetsService.Spreadsheets.Values.Get(c.sheetID, range_).Do()
	if err != nil {
		return fmt.Errorf("could not read data from spreadsheet: %v", err)
	}

	// Store the data in the Client's sheetData field.
	c.sheetData = response.Values
	return nil
}

// saveToken saves the OAuth2 token to a file.
func (c *Client) saveToken(token *oauth2.Token) error {
	jsonToken, err := json.Marshal(token)
	if err != nil {
		return fmt.Errorf("could not marshal token: %v", err)
	}
	return ioutil.WriteFile(c.tokenFile, jsonToken, 0600)
}

// loadToken loads the OAuth2 token from a file.
func (c *Client) loadToken() (*oauth2.Token, error) {
	tokenBytes, err := ioutil.ReadFile(c.tokenFile)
	if err != nil {
		return nil, fmt.Errorf("could not read token file: %v", err)
	}
	token := &oauth2.Token{}
	err = json.Unmarshal(tokenBytes, token)
	if err != nil {
		return nil, fmt.Errorf("could not unmarshal token: %v", err)
	}
	return token, nil
}

// getTokenFromWeb uses an OAuth2 flow to get a token from the user.
func (c *Client) getTokenFromWeb() (*oauth2.Token, error) {
	// Generate a random state token to prevent CSRF attacks.
	state := strconv.Itoa(rand.Intn(10000))
	authURL := c.oauthConfig.AuthCodeURL(state)
	fmt.Printf("Go to the following link in your browser and authorize this application: %v\n", authURL)

	// Open a web browser to the OAuth2 consent page.
	if err := openURL(authURL); err != nil {
		return nil, fmt.Errorf("could not open web browser: %v", err)
	}

	// Wait for the user to authorize the application.
	listener, err := net.Listen("tcp", redirectURI)
	if err != nil {
		return nil, fmt.Errorf("could not listen on redirect URI: %v", err)
	}
	defer listener.Close()
	ch := make(chan string)
	go func() {
		conn, err := listener.Accept()
		if err != nil {
			ch <- fmt.Sprintf("could not accept connection: %v", err)
			return
		}
		scanner := bufio.NewScanner(conn)
		scanner.Scan()
		code := scanner.Text()
		code = strings.Replace(code, "http://localhost:14000/callback/?code=",