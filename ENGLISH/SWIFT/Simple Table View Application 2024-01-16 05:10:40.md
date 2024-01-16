import UIKit

class ViewController: UIViewController {

    private let tableView: UITableView = {
        let tableView = UITableView(frame: .zero, style: .plain)
        tableView.register(UITableViewCell.self, forCellReuseIdentifier: "cell")
        return tableView
    }()

    private let data = ["Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Item 7", "Item 8", "Item 9", "Item 10"]

    override func viewDidLoad() {
        super.viewDidLoad()

        view.addSubview(tableView)
        tableView.dataSource = self
        tableView.delegate = self
    }

    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()

        tableView.frame = view.bounds
    }
}

extension ViewController: UITableViewDataSource {

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return data.count
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "cell", for: indexPath)
        cell.textLabel?.text = data[indexPath.row]
        return cell
    }
}

extension ViewController: UITableViewDelegate {

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        tableView.deselectRow(at: indexPath, animated: true)

        let alert = UIAlertController(title: "Item Selected", message: "You selected item \(data[indexPath.row])", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default, handler: nil))
        present(alert, animated: true, completion: nil)
    }
}

This code creates a simple table view application in Swift. Here's how it works:

1. We start by importing the UIKit framework, which is essential for creating user interfaces in iOS applications.

2. We define a class named ViewController, which is the main controller for our application. It inherits from UIViewController, which is the base class for all view controllers in UIKit.

3. Inside the ViewController class, we create a private property called tableView. This property is an instance of UITableView, which is the class used to display tables in iOS. We also register a reusable cell with the identifier "cell" for use in the table view.

4. We define an array called data, which contains a list of strings representing the items to be displayed in the table view.

5. In the viewDidLoad method, which is called when the view controller is loaded into memory, we add the tableView to the view hierarchy and set its data source and delegate to self. This allows the table view to get data from the ViewController class and to respond to user interactions.

6. In the viewDidLayoutSubviews method, which is called when the layout of the view controller's view is updated, we set the frame of the tableView to match the bounds of the view. This ensures that the table view fills the entire screen.

7. We implement the UITableViewDataSource protocol, which requires us to provide the table view with the number of rows and the cell for each row. In this case, we simply return the number of items in the data array and create a simple cell with a text label for each row.

8. We implement the UITableViewDelegate protocol, which allows us to respond to user interactions with the table view. In this case, we define a method that handles the selection of a row in the table view. When a row is selected, we display an alert with a message indicating which item was selected.

This code creates a basic table view application with a list of items that can be selected. When a user selects an item, an alert is displayed with a message indicating which item was selected.