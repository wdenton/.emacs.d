# My Emacs config

Inspired by https://github.com/magnars/.emacs.d and https://gitcafe.com/Leaflet/.emacs.d/ and others.

If, for some reason I cannot fathom, you want to try out my Emacs configuration, run these commands. First you'll move your existing Emacs configuration out of the way, then you'll clone mine.

    $ mv .emacs .emacs.BAK
	$ mv .emacs.d .emacs.d.BAK
	$ git clone https://github.com/wdenton/.emacs.d.git
	$ emacs

When you run Emacs it will probably spend a while downloading and installing packages. That's all right. When it's done, kill all of the leftover buffers you don't need.  You'll be left with a dark full-screen Emacs that runs like mine does.

`M-x all-praise-emacs`

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

Copyright 2012--2015 William Denton <wtd@pobox.com>
