**Code complexe en Go :**

```go
package main

import (
    "context"
    "fmt"
    "io"
    "log"
    "sync"
    "time"

    "github.com/google/uuid"
    "github.com/golang/protobuf/ptypes"
    "github.com/golang/protobuf/ptypes/timestamp"
    "google.golang.org/grpc"
    "google.golang.org/grpc/codes"
    "google.golang.org/grpc/metadata"
    "google.golang.org/grpc/status"
)

const (
    ServerPort = ":9090"
)

type ServiceClient interface {
    Create(ctx context.Context, req *CreateRequest, opts ...grpc.CallOption) (*CreateResponse, error)
    Read(ctx context.Context, req *ReadRequest, opts ...grpc.CallOption) (*ReadResponse, error)
    Update(ctx context.Context, req *UpdateRequest, opts ...grpc.CallOption) (*UpdateResponse, error)
    Delete(ctx context.Context, req *DeleteRequest, opts ...grpc.CallOption) (*DeleteResponse, error)
}

type ServiceServer struct {
    mux sync.Mutex
    items map[string]*Item
}

type CreateRequest struct {
    Item *Item
}

type CreateResponse struct {
    Id string
}

type ReadRequest struct {
    Id string
}

type ReadResponse struct {
    Item *Item
}

type UpdateRequest struct {
    Item *Item
}

type UpdateResponse struct {
    Success bool
}

type DeleteRequest struct {
    Id string
}

type DeleteResponse struct {
    Success bool
}

type Item struct {
    Id        string
    Name      string
    CreatedAt time.Time
    UpdatedAt time.Time
}

func NewServiceClient(conn *grpc.ClientConn) ServiceClient {
    return &serviceClient{conn}
}

type serviceClient struct {
    conn *grpc.ClientConn
}

func (c *serviceClient) Create(ctx context.Context, req *CreateRequest, opts ...grpc.CallOption) (*CreateResponse, error) {
    client := NewServiceClient(conn)
    ctx, cancel := context.WithTimeout(ctx, 10*time.Second)
    defer cancel()

    req.Item.Id = uuid.New().String()
    req.Item.CreatedAt = time.Now()
    req.Item.UpdatedAt = time.Now()

    resp, err := client.Create(ctx, req)
    if err != nil {
        return nil, err
    }

    return resp, nil
}

func (c *serviceClient) Read(ctx context.Context, req *ReadRequest, opts ...grpc.CallOption) (*ReadResponse, error) {
    client := NewServiceClient(conn)
    ctx, cancel := context.WithTimeout(ctx, 10*time.Second)
    defer cancel()

    resp, err := client.Read(ctx, req)
    if err != nil {
        return nil, err
    }

    return resp, nil
}

func (c *serviceClient) Update(ctx context.Context, req *UpdateRequest, opts ...grpc.CallOption) (*UpdateResponse, error) {
    client := NewServiceClient(conn)
    ctx, cancel := context.WithTimeout(ctx, 10*time.Second)
    defer cancel()

    req.Item.UpdatedAt = time.Now()

    resp, err := client.Update(ctx, req)
    if err != nil {
        return nil, err
    }

    return resp, nil
}

func (c *serviceClient) Delete(ctx context.Context, req *DeleteRequest, opts ...grpc.CallOption) (*DeleteResponse, error) {
    client := NewServiceClient(conn)
    ctx, cancel := context.WithTimeout(ctx, 10*time.Second)
    defer cancel()

    resp, err := client.Delete(ctx, req)
    if err != nil {
        return nil, err
    }

    return resp, nil
}

func NewServiceServer() *ServiceServer {
    return &ServiceServer{
        items: make(map[string]*Item),
    }
}

func (s *ServiceServer) Create(ctx context.Context, req *CreateRequest) (*CreateResponse, error) {
    s.mux.Lock()
    defer s.mux.Unlock()

    item := req.Item
    item.Id = uuid.New().String()
    item.CreatedAt = time.Now()
    item.UpdatedAt = time.Now()

    s.items[item.Id] = item

    return &CreateResponse{
        Id: item.Id,
    }, nil
}

func (s *ServiceServer) Read(ctx context.Context, req *ReadRequest) (*ReadResponse, error) {
    s.mux.Lock()
    defer s.mux.Unlock()

    item, ok := s.items[req.Id]
    if !ok {
        return nil, status.Errorf(codes.NotFound, "item not found")
    }

    return &ReadResponse{
        Item: item,
    }, nil
}

func (s *ServiceServer) Update(ctx context.Context, req *UpdateRequest) (*UpdateResponse, error) {
    s.mux.Lock()
    defer s.mux.Unlock()

    item, ok := s.items[req.Item.Id]
    if !ok {
        return nil, status.Errorf(codes.NotFound, "item not found")
    }

    item.Name = req.Item.Name
    item.UpdatedAt = time.Now()

    return &UpdateResponse{
        Success: true,
    }, nil
}

func (s *ServiceServer) Delete(ctx context.Context, req *DeleteRequest) (*DeleteResponse, error) {
    s.mux.Lock()
    defer s.mux.Unlock()

    _, ok := s.items[req.Id]
    if !ok {
        return nil, status.Errorf(codes.NotFound, "item not found")
    }

    delete(s.items, req.Id)

    return &DeleteResponse{
        Success: true,
    }, nil
}

func main() {
    lis, err := net.Listen("tcp", ServerPort)
    if err != nil {
        log.Fatalf("failed to listen: %v", err)
    }

    grpcServer := grpc.NewServer()
    RegisterServiceServer(grpcServer, NewServiceServer())

    log.