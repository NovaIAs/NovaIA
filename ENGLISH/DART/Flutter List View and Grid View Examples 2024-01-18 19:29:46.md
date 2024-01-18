```dart
import 'package:flutter/material.dart';

class ListViewBuilderExample extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('ListView Builder Example'),
      ),
      body: ListView.builder(
        itemCount: 100,
        itemBuilder: (context, index) {
          return ListTile(
            title: Text('Item $index'),
            subtitle: Text('This is item number $index'),
          );
        },
      ),
    );
  }
}

class GridViewBuilderExample extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('GridView Builder Example'),
      ),
      body: GridView.builder(
        itemCount: 100,
        gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
          crossAxisCount: 3,
        ),
        itemBuilder: (context, index) {
          return Container(
            color: Colors.blue,
            child: Center(
              child: Text('Item $index'),
            ),
          );
        },
      ),
    );
  }
}

class ListViewSeparatedExample extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('ListView Separated Example'),
      ),
      body: ListView.separated(
        itemCount: 100,
        separatorBuilder: (context, index) {
          return Divider();
        },
        itemBuilder: (context, index) {
          return ListTile(
            title: Text('Item $index'),
            subtitle: Text('This is item number $index'),
          );
        },
      ),
    );
  }
}

class GridViewSeparatedExample extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('GridView Separated Example'),
      ),
      body: GridView.separated(
        itemCount: 100,
        gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
          crossAxisCount: 3,
        ),
        separatorBuilder: (context, index) {
          return Divider();
        },
        itemBuilder: (context, index) {
          return Container(
            color: Colors.blue,
            child: Center(
              child: Text('Item $index'),
            ),
          );
        },
      ),
    );
  }
}

class CustomListViewExample extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Custom ListView Example'),
      ),
      body: ListView(
        children: [
          ListTile(
            title: Text('Item 1'),
            subtitle: Text('This is item number 1'),
          ),
          ListTile(
            title: Text('Item 2'),
            subtitle: Text('This is item number 2'),
          ),
          ListTile(
            title: Text('Item 3'),
            subtitle: Text('This is item number 3'),
          ),
        ],
      ),
    );
  }
}

class CustomGridViewExample extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Custom GridView Example'),
      ),
      body: GridView(
        gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
          crossAxisCount: 3,
        ),
        children: [
          Container(
            color: Colors.blue,
            child: Center(
              child: Text('Item 1'),
            ),
          ),
          Container(
            color: Colors.red,
            child: Center(
              child: Text('Item 2'),
            ),
          ),
          Container(
            color: Colors.green,
            child: Center(
              child: Text('Item 3'),
            ),
          ),
        ],
      ),
    );
  }
}

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter List View Example',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: ListViewBuilderExample(),
    );
  }
}
```

Explanation:

This Dart code demonstrates various ways to create list views and grid views in Flutter. It includes examples using ListView.builder, GridView.builder, ListView.separated, GridView.separated, and custom list views and grid views.

1. ListViewBuilderExample:

   - This class creates a simple ListView with 100 items using the ListView.builder constructor.
   - Each item in the list is a ListTile with a title and subtitle.

2. GridViewBuilderExample:

   - This class creates a GridView with 100 items using the GridView.builder constructor.
   - The grid view has three columns and each item is a Container with a blue background and a centered text.

3. ListViewSeparatedExample:

   - This class creates a ListView with 100 items using the ListView.separated constructor.
   - The list view has a Divider between each item.

4. GridViewSeparatedExample:

   - This class creates a GridView with 100 items using the GridView.separated constructor.
   - The grid view has a Divider between each item.

5. CustomListViewExample:

   - This class creates a custom ListView with three items.
   - Each item is a ListTile with a title and subtitle.

6. CustomGridViewExample:

   - This class creates a custom GridView with three items.
   - Each item is a Container with a colored background and a centered text.

7. MyApp:

   - This class is the main application class.
   - It creates a MaterialApp with a ListViewBuilderExample as the home page.

This code showcases different ways to create list views and grid views in Flutter, providing flexibility and customization options for various scenarios.