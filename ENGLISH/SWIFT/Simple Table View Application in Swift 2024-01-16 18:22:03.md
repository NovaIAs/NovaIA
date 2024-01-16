```swift
import UIKit

class MyComplexClass: UIViewController {

    // MARK: - Properties

    private let tableView = UITableView()
    private let data = [
        "Item 1",
        "Item 2",
        "Item 3",
        "Item 4",
        "Item 5",
        "Item 6",
        "Item 7",
        "Item 8",
        "Item 9",
        "Item 10"
    ]

    // MARK: - Lifecycle Methods

    override func viewDidLoad() {
        super.viewDidLoad()

        // Setup the table view
        tableView.dataSource = self
        tableView.delegate = self
        tableView.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(tableView)

        // Setup the constraints for the table view
        NSLayoutConstraint.activate([
            tableView.topAnchor.constraint(equalTo: view.topAnchor),
            tableView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            tableView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            tableView.bottomAnchor.constraint(equalTo: view.bottomAnchor)
        ])
    }

    // MARK: - UITableViewDataSource Methods

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return data.count
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = UITableViewCell(style: .default, reuseIdentifier: "Cell")
        cell.textLabel?.text = data[indexPath.row]
        return cell
    }

    // MARK: - UITableViewDelegate Methods

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let item = data[indexPath.row]
        let alert = UIAlertController(title: "Item Selected", message: "You selected \(item)", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default, handler: nil))
        present(alert, animated: true)
    }
}
```

This code creates a simple table view application in Swift. The table view is populated with data from an array, and when a row is selected, an alert is shown.

The code is divided into three sections:

* **Properties:** This section declares the properties of the class. In this case, there is a private table view and an array of data.
* **Lifecycle Methods:** This section contains the lifecycle methods of the class. The `viewDidLoad()` method is called when the view is loaded, and it is used to set up the table view and its constraints.
* **UITableViewDataSource Methods:** This section contains the methods that are required to implement the `UITableViewDataSource` protocol. These methods are used to provide data to the table view.
* **UITableViewDelegate Methods:** This section contains the methods that are required to implement the `UITableViewDelegate` protocol. These methods are used to handle events that occur on the table view, such as when a row is selected.

The code is well-commented and easy to understand. It is also well-structured, with each section of the code being clearly separated.