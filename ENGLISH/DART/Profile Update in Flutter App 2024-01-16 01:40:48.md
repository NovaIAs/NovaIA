```dart
import 'package:flutter/material.dart';
import 'package:firebase_auth/firebase_auth.dart';
import 'package:cloud_firestore/cloud_firestore.dart';
import 'package:fluttertoast/fluttertoast.dart';
import 'package:intl/intl.dart';

class ProfileScreen extends StatefulWidget {
  @override
  _ProfileScreenState createState() => _ProfileScreenState();
}

class _ProfileScreenState extends State<ProfileScreen> {
  // Firebase Auth instance.
  final FirebaseAuth _auth = FirebaseAuth.instance;
  // Firebase Firestore instance.
  final FirebaseFirestore _firestore = FirebaseFirestore.instance;

  // User's data.
  String _name = '';
  String _email = '';
  String _phoneNumber = '';
  String _birthDate = '';
  String _gender = '';

  // Flag to check if the user is already updating their profile.
  bool _isUpdating = false;

  // TextEditingControllers to get the user's input.
  final TextEditingController _nameController = TextEditingController();
  final TextEditingController _emailController = TextEditingController();
  final TextEditingController _phoneNumberController = TextEditingController();
  final TextEditingController _birthDateController = TextEditingController();
  final TextEditingController _genderController = TextEditingController();

  // Date formatters.
  final DateFormat _dateFormat = DateFormat('dd/MM/yyyy');
  final DateFormat _monthFormat = DateFormat('MMMM');

  @override
  void initState() {
    super.initState();
    // Get the user's data from the database.
    _getUserData();
  }

  @override
  void dispose() {
    _nameController.dispose();
    _emailController.dispose();
    _phoneNumberController.dispose();
    _birthDateController.dispose();
    _genderController.dispose();
    super.dispose();
  }

  // Get the user's data from the database.
  void _getUserData() async {
    // Get the user's UID.
    String uid = _auth.currentUser.uid;

    // Get the user's document from the database.
    DocumentSnapshot documentSnapshot = await _firestore.collection('users').doc(uid).get();

    // Get the user's data from the document.
    Map<String, dynamic> data = documentSnapshot.data();

    // Set the user's data in the TextControllers.
    _nameController.text = data['name'];
    _emailController.text = data['email'];
    _phoneNumberController.text = data['phoneNumber'];
    _birthDateController.text = _dateFormat.format(data['birthDate'].toDate());
    _genderController.text = data['gender'];

    // Set the user's data in the state variables.
    _name = data['name'];
    _email = data['email'];
    _phoneNumber = data['phoneNumber'];
    _birthDate = _dateFormat.format(data['birthDate'].toDate());
    _gender = data['gender'];
  }

  // Update the user's profile.
  void _updateProfile() async {
    // Set the flag to true to disable the update button.
    _isUpdating = true;

    // Get the user's UID.
    String uid = _auth.currentUser.uid;

    // Create a document reference for the user's document in the database.
    DocumentReference documentReference = _firestore.collection('users').doc(uid);

    // Create a map with the user's data.
    Map<String, dynamic> data = {
      'name': _nameController.text,
      'email': _emailController.text,
      'phoneNumber': _phoneNumberController.text,
      'birthDate': _birthDateController.text,
      'gender': _genderController.text,
    };

    // Update the user's document in the database.
    await documentReference.update(data);

    // Set the flag to false to enable the update button.
    _isUpdating = false;

    // Show a toast message to the user.
    Fluttertoast.showToast(
      msg: 'Profile updated successfully',
      toastLength: Toast.LENGTH_SHORT,
      gravity: ToastGravity.BOTTOM,
      backgroundColor: Colors.green,
      textColor: Colors.white,
      fontSize: 16.0,
    );
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Profile'),
      ),
      body: Padding(
        padding: EdgeInsets.all(16.0),
        child: SingleChildScrollView(
          child: Column(
            children: [
              // Name field.
              TextField(
                controller: _nameController,
                decoration: InputDecoration(
                  labelText: 'Name',
                ),
              ),
              SizedBox(height: 16.0),

              // Email field.
              TextField(
                controller: _emailController,
                decoration: InputDecoration(
                  labelText: 'Email',
                ),
              ),
              SizedBox(height: 16.0),

              // Phone number field.
              TextField(
                controller: _phoneNumberController,
                decoration: InputDecoration(
                  labelText: 'Phone Number',
                ),
              ),
              SizedBox(height: 16.0),

              // Birth date field.
              TextField(
                controller: _birthDateController,
                decoration: InputDecoration(
                  labelText: 'Birth Date',
                  suffixIcon: IconButton(
                    icon: Icon(Icons.calendar_today),
                    onPressed: () async {
                      // Show a date picker.
                      DateTime? date = await showDatePicker(
                        context: context,
                        initialDate: DateTime.now(),
                        firstDate: DateTime(1900),
                        lastDate: DateTime.now(),
                      );

                      // If the user selected a date, set it in the birth date field.
                      if (date != null) {
                        _birthDateController.text = _dateFormat.format(date);
                      }
                    },
                  ),
                ),
              ),
              SizedBox(height: 16.0),

              // Gender field.
              TextField(
                controller: _genderController,
                decoration: InputDecoration(
                  labelText: 'Gender',
                ),
              ),
              SizedBox(height: 16.0),

              // Update button.
              ElevatedButton(
                onPressed: _isUpdating ? null : _updateProfile,
                child: Text('Update Profile'),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
```

This code creates a profile screen in a Flutter app. The user can edit their name, email, phone number, birth date, and gender. The app uses Firebase Auth to get the user's UID and Firebase Firestore to get and update the user's data. The code is well-commented and easy to understand.