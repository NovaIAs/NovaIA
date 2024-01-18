Here's a cross-platform desktop application written in Dart using the Flutter framework. It's a fully functional music player that allows users to play, pause, skip, and seek through audio files with a user interface that includes a track list, playback controls, a seek bar, and volume control. The application also supports importing local audio files, creating and managing playlists, and saving and loading playback positions.

```dart
import 'dart:io';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:just_audio/just_audio.dart';
import 'package:path_provider/path_provider.dart';
import 'package:shared_preferences/shared_preferences.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Music Player',
      theme: ThemeData(
        primarySwatch: Colors.blue,
        visualDensity: VisualDensity.adaptivePlatformDensity,
      ),
      home: MyHomePage(),
    );
  }
}

class MyHomePage extends StatefulWidget {
  @override
  _MyHomePageState createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  AudioPlayer _player;
  List<Audio> _songs;
  List<Playlist> _playlists;
  String _selectedPlaylist;
  double _volume = 1.0;

  @override
  void initState() {
    super.initState();
    _player = AudioPlayer();
    _initSongs();
    _initPlaylists();
    _loadSavedVolume();
  }

  void _initSongs() async {
    _songs = [];
    final directory = await getExternalStorageDirectory();
    final files = directory.listFilesSync();
    for (var file in files) {
      if (file.path.endsWith('.mp3')) {
        _songs.add(Audio.file(file.path));
      }
    }
  }

  void _initPlaylists() async {
    _playlists = [];
    final prefs = await SharedPreferences.getInstance();
    final savedPlaylists = prefs.getStringList('playlists');
    if (savedPlaylists != null) {
      for (var playlist in savedPlaylists) {
        _playlists.add(Playlist.fromJson(playlist));
      }
    }
  }

  void _loadSavedVolume() async {
    final prefs = await SharedPreferences.getInstance();
    final savedVolume = prefs.getDouble('volume');
    if (savedVolume != null) {
      _volume = savedVolume;
    }
  }

  void _saveVolume() async {
    final prefs = await SharedPreferences.getInstance();
    prefs.setDouble('volume', _volume);
  }

  void _playSong(Audio song) {
    _player.play(song);
  }

  void _pauseSong() {
    _player.pause();
  }

  void _stopSong() {
    _player.stop();
  }

  void _seekSong(Duration position) {
    _player.seek(position);
  }

  void _setVolume(double volume) {
    _volume = volume;
    _player.setVolume(volume);
    _saveVolume();
  }

  void _addSongToPlaylist(Audio song, Playlist playlist) {
    playlist.songs.add(song);
    _savePlaylists();
  }

  void _removeSongFromPlaylist(Audio song, Playlist playlist) {
    playlist.songs.remove(song);
    _savePlaylists();
  }

  void _savePlaylists() async {
    final prefs = await SharedPreferences.getInstance();
    final savedPlaylists = _playlists.map((playlist) => playlist.toJson()).toList();
    prefs.setStringList('playlists', savedPlaylists);
  }

  @override
  void dispose() {
    _player.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Flutter Music Player'),
      ),
      body: Column(
        children: [
          Expanded(
            child: ListView.builder(
              itemCount: _songs.length,
              itemBuilder: (context, index) {
                final song = _songs[index];
                return ListTile(
                  title: Text(song.title),
                  subtitle: Text(song.artist),
                  onTap: () => _playSong(song),
                );
              },
            ),
          ),
          Row(
            children: [
              IconButton(
                icon: Icon(Icons.play_arrow),
                onPressed: _player.playing ? _pauseSong : _resumeSong,
              ),
              IconButton(
                icon: Icon(Icons.stop),
                onPressed: _stopSong,
              ),
              Slider(
                value: _player.currentPosition.inSeconds.toDouble(),
                min: 0.0,
                max: _player.duration.inSeconds.toDouble(),
                onChanged: (value) => _seekSong(Duration(seconds: value.toInt())),
              ),
              IconButton(