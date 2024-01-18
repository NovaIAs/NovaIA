```go
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"

	"cloud.google.com/go/compute/apiv1"
	computepb "google.golang.org/genproto/googleapis/cloud/compute/v1"
	"google.golang.org/genproto/googleapis/protobuf/field_mask"
)

func main() {
	// Load variables from environment.
	projectID := os.Getenv("GOOGLE_CLOUD_PROJECT")
	zone := os.Getenv("COMPUTE_ZONE")
	instanceName := os.Getenv("INSTANCE_NAME")
	port := os.Getenv("PORT")
	if port == "" {
		port = "8080"
	}

	// Allocate an external IP address for the instance.
	externalIP, err := allocateExternalIP(projectID, zone)
	if err != nil {
		log.Fatalf("failed to allocate external IP: %v", err)
	}
	defer releaseExternalIP(projectID, zone, externalIP)

	// Start a webserver on the instance.
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello, this is instance %s running on IP %s!\n", instanceName, externalIP)
	})
	go func() {
		log.Fatal(http.ListenAndServe(":"+port, nil))
	}()

	// Create a firewall rule to allow HTTP traffic to the instance.
	firewallRuleName := instanceName + "-http-allow"
	if err := createFirewallRule(projectID, firewallRuleName, zone, externalIP, port); err != nil {
		log.Fatalf("failed to create firewall rule: %v", err)
	}
	defer deleteFirewallRule(projectID, firewallRuleName)

	// Wait for the instance to become running.
	if err := waitForInstance(projectID, zone, instanceName); err != nil {
		log.Fatalf("failed to wait for instance: %v", err)
	}

	// Get the instance's public IPv4 address.
	instanceIP, err := getInstanceIP(projectID, zone, instanceName)
	if err != nil {
		log.Fatalf("failed to get instance IP: %v", err)
	}

	// Create a Cloud NAT rule to forward traffic to the instance.
	natRuleName := instanceName + "-nat"
	if err := createNatRule(projectID, zone, natRuleName, instanceIP, externalIP); err != nil {
		log.Fatalf("failed to create NAT rule: %v", err)
	}
	defer deleteNatRule(projectID, zone, natRuleName)

	// Send a request to the instance to verify that it is reachable.
	url := fmt.Sprintf("http://%s:%s/", instanceIP, port)
	resp, err := http.Get(url)
	if err != nil {
		log.Fatalf("failed to send request: %v", err)
	}
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatalf("failed to read response body: %v", err)
	}
	fmt.Printf("Received response: %s\n", string(body))

	// Wait for the user to press enter to shut down the instance.
	fmt.Print("Press enter to shut down the instance...")
	fmt.Scanln()

	// Shut down the instance.
	if err := stopInstance(projectID, zone, instanceName); err != nil {
		log.Fatalf("failed to stop instance: %v", err)
	}
}

// allocateExternalIP allocates an external IP address for the instance.
func allocateExternalIP(projectID, zone string) (string, error) {
	ctx := context.Background()
	instancesClient, err := compute.NewInstancesRESTClient(ctx)
	if err != nil {
		return "", fmt.Errorf("NewInstancesRESTClient: %v", err)
	}
	defer instancesClient.Close()

	req := &computepb.AllocateAddressRequest{
		Project: projectID,
		Zone:    zone,
	}
	op, err := instancesClient.AllocateAddress(ctx, req)
	if err != nil {
		return "", fmt.Errorf("AllocateAddress: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return "", fmt.Errorf("Wait: %v", err)
	}
	return op.Response.(*computepb.Address).Address, nil
}

// releaseExternalIP releases the external IP address for the instance.
func releaseExternalIP(projectID, zone, externalIP string) error {
	ctx := context.Background()
	instancesClient, err := compute.NewInstancesRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewInstancesRESTClient: %v", err)
	}
	defer instancesClient.Close()

	req := &computepb.DeleteAddressRequest{
		Project: projectID,
		Zone:    zone,
		Address: externalIP,
	}
	op, err := instancesClient.DeleteAddress(ctx, req)
	if err != nil {
		return fmt.Errorf("DeleteAddress: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}
	return nil
}

// createFirewallRule creates a firewall rule to allow HTTP traffic to the instance.
func createFirewallRule(projectID, firewallRuleName, zone, externalIP, port string) error {
	ctx := context.Background()
	firewallsClient, err := compute.NewFirewallsRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewFirewallsRESTClient: %v", err)
	}
	defer firewallsClient.Close()

	firewallRule := &computepb.Firewall{
		Name:    firewallRuleName,
		Network: "global/networks/default",
		Allowed: []*computepb.Allowed{
			{
				IPProtocol: "tcp",
				Ports:      []string{port},
			},
		},
		Direction: "INGRESS",
		TargetTags: []string{
			instanceName,
		},
		SourceRanges: []string{
			"0.0.0.0/0",
		},
	}
	req := &computepb.InsertFirewallRequest{
		FirewallResource: firewallRule,
		Project:          projectID,
	}
	op, err := firewallsClient.Insert(ctx, req)
	if err != nil {
		return fmt.Errorf("Insert: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}
	return nil
}

// deleteFirewallRule deletes the firewall rule.
func deleteFirewallRule(projectID, firewallRuleName string) error {
	ctx := context.Background()
	firewallsClient, err := compute.NewFirewallsRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewFirewallsRESTClient: %v", err)
	}
	defer firewallsClient.Close()

	req := &computepb.DeleteFirewallRequest{
		Firewall: firewallRuleName,
		Project:  projectID,
	}
	op, err := firewallsClient.Delete(ctx, req)
	if err != nil {
		return fmt.Errorf("Delete: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}
	return nil
}

// waitForInstance waits for the instance to become running.
func waitForInstance(projectID, zone, instanceName string) error {
	ctx := context.Background()
	instancesClient, err := compute.NewInstancesRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewInstancesRESTClient: %v", err)
	}
	defer instancesClient.Close()

	for {
		req := &computepb.GetInstanceRequest{
			Project:  projectID,
			Zone:     zone,
			Instance: instanceName,
		}
		instance, err := instancesClient.Get(ctx, req)
		if err != nil {
			return fmt.Errorf("Get: %v", err)
		}
		if instance.Status == "RUNNING" {
			return nil
		}
		time.Sleep(5 * time.Second)
	}
}

// getInstanceIP gets the instance's public IPv4 address.
func getInstanceIP(projectID, zone, instanceName string) (string, error) {
	ctx := context.Background()
	instancesClient, err := compute.NewInstancesRESTClient(ctx)
	if err != nil {
		return "", fmt.Errorf("NewInstancesRESTClient: %v", err)
	}
	defer instancesClient.Close()

	req := &computepb.GetInstanceRequest{
		Project:  projectID,
		Zone:     zone,
		Instance: instanceName,
	}
	instance, err := instancesClient.Get(ctx, req)
	if err != nil {
		return "", fmt.Errorf("Get: %v", err)
	}
	if len(instance.NetworkInterfaces) == 0 {
		return "", fmt.Errorf("no network interfaces found for instance")
	}
	return instance.NetworkInterfaces[0].NetworkIP, nil
}

// createNatRule creates a Cloud NAT rule to forward traffic to the instance.
func createNatRule(projectID, zone, natRuleName, instanceIP, externalIP string) error {
	ctx := context.Background()
	natClient, err := compute.NewRegionUrlMapsRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewRegionUrlMapsRESTClient: %v", err)
	}
	defer natClient.Close()

	natRule := &computepb.UrlMap{
		Name:           natRuleName,
		DefaultService: instanceIP,
	}
	req := &computepb.InsertRegionUrlMapRequest{
		UrlMapResource: natRule,
		Project:        projectID,
		Region:         zone,
	}
	op, err := natClient.Insert(ctx, req)
	if err != nil {
		return fmt.Errorf("Insert: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}

	forwardingRule := &computepb.ForwardingRule{
		Name:       natRuleName,
		IPProtocol: "TCP",
		PortRange:   "80",
		Region:      zone,
		Target:      natRule.SelfLink,
	}

	req = &computepb.InsertForwardingRuleRequest{
		ForwardingRuleResource: forwardingRule,
		Project:               projectID,
		Region:                zone,
	}
	op, err = natClient.InsertForwardingRule(ctx, req)
	if err != nil {
		return fmt.Errorf("InsertForwardingRule: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}

	return nil
}

// deleteNatRule deletes the Cloud NAT rule.
func deleteNatRule(projectID, zone, natRuleName string) error {
	ctx := context.Background()
	natClient, err := compute.NewRegionUrlMapsRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewRegionUrlMapsRESTClient: %v", err)
	}
	defer natClient.Close()

	req := &computepb.DeleteRegionUrlMapRequest{
		UrlMap:  natRuleName,
		Project: projectID,
		Region:  zone,
	}
	op, err := natClient.Delete(ctx, req)
	if err != nil {
		return fmt.Errorf("Delete: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}

	req = &computepb.DeleteForwardingRuleRequest{
		ForwardingRule: natRuleName,
		Project:       projectID,
		Region:        zone,
	}
	op, err = natClient.DeleteForwardingRule(ctx, req)
	if err != nil {
		return fmt.Errorf("DeleteForwardingRule: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}

	return nil
}

// stopInstance stops the instance.
func stopInstance(projectID, zone, instanceName string) error {
	ctx := context.Background()
	instancesClient, err := compute.NewInstancesRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewInstancesRESTClient: %v", err)
	}
	defer instancesClient.Close()

	req := &computepb.StopInstanceRequest{
		Project:  projectID,
		Zone:     zone,
		Instance: instanceName,
	}
	op, err := instancesClient.Stop(ctx, req)
	if err != nil {
		return fmt.Errorf("Stop: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}
	return nil
}

// startInstance starts the instance.
func startInstance(projectID, zone, instanceName string) error {
	ctx := context.Background()
	instancesClient, err := compute.NewInstancesRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewInstancesRESTClient: %v", err)
	}
	defer instancesClient.Close()

	req := &computepb.StartInstanceRequest{
		Project:  projectID,
		Zone:     zone,
		Instance: instanceName,
	}
	op, err := instancesClient.Start(ctx, req)
	if err != nil {
		return fmt.Errorf("Start: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}
	return nil
}

// deleteInstance deletes the instance.
func deleteInstance(projectID, zone, instanceName string) error {
	ctx := context.Background()
	instancesClient, err := compute.NewInstancesRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewInstancesRESTClient: %v", err)
	}
	defer instancesClient.Close()

	req := &computepb.DeleteInstanceRequest{
		Project:  projectID,
		Zone:     zone,
		Instance: instanceName,
	}
	op, err := instancesClient.Delete(ctx, req)
	if err != nil {
		return fmt.Errorf("Delete: %v", err)
	}
	if err := op.Wait(ctx); err != nil {
		return fmt.Errorf("Wait: %v", err)
	}
	return nil
}

// insertInstance inserts a new instance.
func insertInstance(projectID, zone, instanceName, machineType, sourceImage, networkName string, publicIP bool) error {
	ctx := context.Background()
	instancesClient, err := compute.NewInstancesRESTClient(ctx)
	if err != nil {
		return fmt.Errorf("NewInstancesRESTClient: %v",