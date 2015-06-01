typeset -U path
path=(~/belguim ~/belgium/**/* ~/.cabal/bin $path)

export EDITOR="/usr/bin/nvim"

export XDG_CONFIG_HOME=$HOME/local/config
export XDG_CACHE_HOME=$HOME/local/cache
export XDG_RUNTIME_HOME=$HOME/local/run

export XDG_DESKTOP_DIR="/home/corr/"
export XDG_DOWNLOAD_DIR="/home/corr/"
export XDG_TEMPLATES_DIR="/home/corr/"
export XDG_PUBLICSHARE_DIR="/home/corr/"
export XDG_DOCUMENTS_DIR="/home/corr/estonia"
export XDG_MUSIC_DIR="/home/corr/france/audio"
export XDG_PICTURES_DIR="/home/corr/france/pictures"
export XDG_VIDEOS_DIR="/home/corr/france/video"

# make Qt look like better
export QT_PLUGIN_PATH=/usr/lib/kde4/plugins/
export QT_PLUGIN_PATH=/usr/lib/kde/plugins/
#export QT_STYLE_OVERRIDE=GTK+

export RUBYLIB=~/local/lib/ruby/

# make applications use wayland, if possible
# uncomment if wayland is running
#export QT_QPA_PLATFORM=wayland-egl
#export GDK_BACKEND=wayland
#export CLUTTER_BACKEND=wayland
#export SDL_VIDEODRIVER=wayland
