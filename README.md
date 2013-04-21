# My Emacs config

Inspired by https://github.com/magnars/.emacs.d and https://gitcafe.com/Leaflet/.emacs.d/ and others.

If, for some reason I cannot fathom, you want to try out my Emacs configuration, run these commands. First you'll move your existing Emacs configuration out of the way, then you'll clone mine.

    $ mv .emacs .emacs.BAK
	$ mv .emacs.d .emacs.d.BAK
	$ git clone https://github.com/wdenton/.emacs.d.git
	$ emacs -mm

When you run Emacs (the `-mm` maximizes the window) it will probably spend a while downloading and installing packages. That's all right. When it's done, kill all of the leftover buffers you don't need.  You'll be left with a dark full-screen Emacs that runs like mine does.

`M-x all-praise-emacs`


