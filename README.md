# zbucium-stump
StumpWM interface for the [zbucium](https://github.com/mihaiolteanu/zbucium)
last.fm music player. No actual music files needed on your pc. It's all played
from youtube with mpv and info from last.fm.

# Install

Follow the steps from the zbucium's README to install the music player first. More importantly, you will need a last.fm account and an API key.

After that,
```bash
# Clone to stumpwm's modules folder 
git clone https://github.com/mihaiolteanu/zbucium-stump ~/stumpwm.d/modules/zbucium-stump
```

```common-lisp
; And load the module in your init file
(load-module "zbucium-stump")
```

# Usage

This is an interface for the above mentioned music player. My greatest wish was
to be able to play last.fm, in the background, without ever having to leave
Emacs again. That means, I would be able to skip songs, see what's playing, add
the current playing song to the list of loved songs, change the 'radio' by
playing a single artist, similar artists, my loved songs or playing songs from a
given genre. On top of that, see the lyrics, again, without leaving the
editor. And best of all, search through all the songs that I've ever played
(with this player, at least) for that tune that keeps bugging me but the artist
of which eludes me. That is, ask the player for all the songs that have a given
lyrics and then select and play the one that seems to best fit it from a list of
candidates, interactively. And if I ever want to leave Emacs, I can open the
browser with the currently playing song's youtube video at any time with the
press of a key.

Enough talk!

![zbucium play artist](https://user-images.githubusercontent.com/8273519/59374522-b2520600-8d54-11e9-97c2-8cd21f5aa5c5.gif)

What happened? First, I'm calling the stumpwm command defined by zbucium's
module, zbucium-play-artist. I'm only calling this as such for illustration
purposes. Normally you'll bind this command to a key combination. It asks me for
an artist and if I want random(i.e. shuffle) play or not. The next command is
zbucium-what-is-playing, which, surely enough, shows me the top song from this
artist playing in the background. The next command is `zbucium-lyrics`.

Here is another nice one. I can choose to play an album. Again it asks me for an
artist (non random by default) and displays the albums for this artist. If I
would to chose one at this stage, it will play it, starting from the first song
till the last, and back again. But I can also browse the album contents, go back
to the album page, chose another album (`C-l`), etc. In this case, I've chosen to play a
single song, from one of the albums.

![zbucium play album](https://user-images.githubusercontent.com/8273519/59374519-b1b96f80-8d54-11e9-8963-43c9b50c687b.gif)

All the commands expected from a player are available. Stop, pause, skip song
and seek, either as standalone command (+- 5 seconds seek, for exemple), or interactively.

![zbucium seek song](https://user-images.githubusercontent.com/8273519/59374524-b2520600-8d54-11e9-8a81-bcb8cca79250.gif)

The same song is playing. You can go forward or backward, in real time (with the
`,` and '.' keys, respectively). As you can see, the timings are not quite
right. This is a limitation of the player used, mpv, which is clearly stated in
their documentation as being only an approximation. Still, good enough for small
searches.

Every song played also saves the lyrics. So let's try to play a song, by
searching it's lyrics.

![zbucium search song](https://user-images.githubusercontent.com/8273519/59374523-b2520600-8d54-11e9-8fb0-5b13d3d05dbe.gif)

The player is stopped initially. Suddently, I have this `i feel i know you` tune
in my head. Who is playing that?! Fire up the song search, put in the verse, or
part of it, and select a song from the list of candidates (if they're too many,
you can begin writing something to filter them in real time). Sure enough, after
the selection, the right song is now playing.

But, the best of all, is that you can start playing some genre or similar
artists and leave it on forever. That's a nice way to discover new music.

# Zbucium Commands (API)

These are all commands defined by zbucium. You can call them directly or,
better, have some keybindings for them.

**zbucium-play-song** _artist_

    Play a single song, over and over again.

**zbucium-play-artist** _artist random_

    Play *ntracks* (see the configuration bellow) for the given artist. Can be
    played random.

**zbucium-play-album** _artist_

    Play a single album for the given artist. While selecting the album, `C-l`
    can be used to peek inside the album tracklist and then go back to the album
    list. So this command can actually play either an album or a single song
    from an album.

**zbucium-play-tag** _tagname random_

    Play the best *ntracks* (see configuration) for the given tag.

**zbucium-play-user-songs** _username random_

    Play *user-ntracks* (see configuration) numbers of songs from the given user name.

**zbucium-play-my-loved-songs** _random_

    Same as above, only it uses the default configured username from the lastfm library.

**zbucium-play-artist-similar** _artist_

    Play artists similar to the given artist. The number of artists considered
    can be configured by setting the *nartists* parameter. Each song is picked
    by first chosing a random artist similar to the given artist and then
    chosing a random song from this artist.

**zbucium-play-tag-similar** _tag_

    Similar with zbucium-play-tag, but I've had better results with this
    one. The toptracks for a given tag are sometimes taken up by a single artist
    so there no actual diversity there. With this command, a random artist is
    picked from the list of artists most representative for this tag. And from
    that artist a random song is selected.

**zbucium-what-is-playing**

    Display the currently playing artist and songs.

**zbucium-lyrics**

    Display the currentply playing songs' lyrics.

**zbucium-love-song**

    Add the currently playing song to the list of loved songs for the user
    configured by the lastfm library

**zbucium-unlove-song**

    The reverse of the above.

**zbucium-next-song**

    Play the next song.

**zbucium-stop**

    Stop playing and go do something else with your day.

**zbucium-search-song** _lyrics_

    Return a list of artist / song / lyrics verse line that matches the string
    supplied by the user. On choosing one entry in the list, that artist's song
    is played.

**zbucium-play/pause**

    Toggle the play/pause status.

**zbucium-replay**

    Replay the song from the beginning

**zbucium-seek-forward**

    Go back 5 seconds.

**zbucium-seek-backward**

    Go forward 5 seconds.

**zbucium-percent-pos**

    Song position, in %. Not very accurate.

**zbucium-seek-song**

    This is the interactive forward or backward song seeking, and it is actually
    a Stump [interactive
    keymap](https://stumpwm.github.io/git/stumpwm-git_3.html). After entering
    it, `,` and '.' are used to seek backwards and forwards, respectively. `C-g`
    quits the interactive keymap.

**zbucium-time-pos**

    Return the current song position, in seconds, from the beginning. This is
    returned by mpv, which is imprecise.

**zbucium-switch-to-browser**

    Pause the player (mpv) and open the default browser with the youtube page
    for the currently playing song.

**zbucium-turn-video-on**

    This will exit the current player instance (mpv) and restart it with video
    support, continuing from where it left off.

# Configuration

The number of songs or artists taken into consideration when playing something
can be configured. Just set these variables in your init file, otherwise, the
default values are used. More can be added in the future if they can enhance the
listening experience in any way.

**\*ntracks\*** _20_ 

    Number of tracks to request from lastfm. These are the first best number of
    tracks for the given artist, according to the last.fm charts.
    
**\*user-ntracks\*** _500_

    Number of songs to consider when playing a list of loved songs.
    
**\*nartists\*** _20_

    Number of artists to request from lastfm. These are used when playing similar
    artists or artists with a given tag.

# Keybindings

This is just an example to get you started. I'm using my leader key, `C-t` and
bind the zbucium map to `m`. So asking for the lyrics would be `C-t m l`. Modify
your init file to suit your wants.

```common-lisp
(define-key *top-map* (kbd "XF86AudioPlay") "zbucium-play/pause")
(define-key *top-map* (kbd "XF86AudioPrev") "zbucium-replay")
(define-key *top-map* (kbd "XF86AudioNext") "zbucium-next-song")
(define-key *top-map* (kbd "XF86AudioStop") "zbucium-stop")

(defvar *zbucium-map* (make-sparse-keymap))
(define-key *root-map* (kbd "m") '*zbucium-map*)

(define-key *zbucium-map* (kbd "v") "zbucium-turn-video-on")
(define-key *zbucium-map* (kbd "f") "zbucium-switch-to-browser")
(define-key *zbucium-map* (kbd "w") "zbucium-what-is-playing")
(define-key *zbucium-map* (kbd "l") "zbucium-lyrics")
(define-key *zbucium-map* (kbd "s") "zbucium-search-song")
(define-key *zbucium-map* (kbd "p") "zbucium-play-artist")
(define-key *zbucium-map* (kbd ",") "zbucium-seek-backward")
(define-key *zbucium-map* (kbd ".") "zbucium-seek-forward")
(define-key *zbucium-map* (kbd "n") "zbucium-next-song")

```

## Authors
Copyright (c) 2019 [Mihai Olteanu](www.mihaiolteanu.me)

Licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) license.
