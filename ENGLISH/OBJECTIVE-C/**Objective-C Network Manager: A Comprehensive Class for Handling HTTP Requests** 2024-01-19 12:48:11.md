```objective-c
#import <Foundation/Foundation.h>

// Define a macro for convenience
#define DEBUG_LOG(format, ...) NSLog((@"%s:%d " format), __PRETTY_FUNCTION__, __LINE__, ##__VA_ARGS__)

// Define a custom error domain
static NSString *const MyErrorDomain = @"com.example.myerror";

// Define a block type for a completion handler
typedef void (^CompletionHandler)(NSError *);

// Create a class to encapsulate the network operations
@interface NetworkManager : NSObject

// A method to perform a GET request
- (void)GET:(NSString *)url completion:(CompletionHandler)completion;

// A method to perform a POST request
- (void)POST:(NSString *)url parameters:(NSDictionary *)parameters completion:(CompletionHandler)completion;

// A method to perform a PUT request
- (void)PUT:(NSString *)url parameters:(NSDictionary *)parameters completion:(CompletionHandler)completion;

// A method to perform a DELETE request
- (void)DELETE:(NSString *)url completion:(CompletionHandler)completion;

@end

// Implement the NetworkManager class
@implementation NetworkManager

// Implement the GET method
- (void)GET:(NSString *)url completion:(CompletionHandler)completion {
    NSURL *requestURL = [NSURL URLWithString:url];
    NSURLRequest *request = [NSURLRequest requestWithURL:requestURL];
    NSURLSession *session = [NSURLSession sharedSession];
    NSURLSessionDataTask *task = [session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            completion(error);
        } else {
            completion(nil);
        }
    }];
    [task resume];
}

// Implement the POST method
- (void)POST:(NSString *)url parameters:(NSDictionary *)parameters completion:(CompletionHandler)completion {
    NSURL *requestURL = [NSURL URLWithString:url];
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:requestURL];
    request.HTTPMethod = @"POST";
    request.HTTPBody = [NSJSONSerialization dataWithJSONObject:parameters options:0 error:nil];
    NSURLSession *session = [NSURLSession sharedSession];
    NSURLSessionDataTask *task = [session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            completion(error);
        } else {
            completion(nil);
        }
    }];
    [task resume];
}

// Implement the PUT method
- (void)PUT:(NSString *)url parameters:(NSDictionary *)parameters completion:(CompletionHandler)completion {
    NSURL *requestURL = [NSURL URLWithString:url];
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:requestURL];
    request.HTTPMethod = @"PUT";
    request.HTTPBody = [NSJSONSerialization dataWithJSONObject:parameters options:0 error:nil];
    NSURLSession *session = [NSURLSession sharedSession];
    NSURLSessionDataTask *task = [session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            completion(error);
        } else {
            completion(nil);
        }
    }];
    [task resume];
}

// Implement the DELETE method
- (void)DELETE:(NSString *)url completion:(CompletionHandler)completion {
    NSURL *requestURL = [NSURL URLWithStrin