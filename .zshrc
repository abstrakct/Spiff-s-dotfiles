# Oh my Zsh!

ZSH=$HOME/.oh-my-zsh
# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="flazz"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vi-mode)
source $ZSH/oh-my-zsh.sh

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=50000
SAVEHIST=50000
#setopt appendhistory autocd
setopt autocd
unsetopt beep
#bindkey -e
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/rolf/.zshrc'

autoload zmv

autoload -Uz compinit
compinit
# End of lines added by compinstall

#autoload -U promptinit
#promptinit
#prompt adam1


# options
setopt extendedglob
setopt histignoredups
setopt completealiases

# variables
export EDITOR=vim
export VISUAL=vim
export LESS="-R"
#export LS_COLORS="di=94:fi=0:ln=36:pi=90:so=35:bd=33:cd=33:or=5;31:mi=5;31:ex=0;32:*.jpg=35:*.gif=35:*.png=35:*.mp3=30;41:*.m4a=30;41:*.ogg=0;41:*.flac=30;42:*.wav=30;42:*.c=93:*.h=91:*.cc=93:*.cpp=93:*.hpp=91:*.hs=93"


# completion for PID!
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*' force-list always

zstyle ':completion:*:pacman:*' force-list always
zstyle ':completion:*:*:pacman:*' menu yes select

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'


# keybindings
bindkey "\e[1~" beginning-of-line # Home
bindkey "\e[4~" end-of-line # End
bindkey "\e[5~" beginning-of-history # PageUp
bindkey "\e[6~" end-of-history # PageDown
bindkey "\e[2~" quoted-insert # Ins
bindkey "\e[3~" delete-char # Del
bindkey "\e[7~" beginning-of-line # Home
bindkey "\e[8~" end-of-line # End
#bindkey "^[[A" history-search-backward
#bindkey "^[[B" history-search-forward


# aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
#alias c3="cal -3m"
alias cal="cal -m"
#alias chat=weechat-curses
alias df="df -h"
alias diff=colordiff
alias el=elinks
alias flexget='nocorrect flexget'
alias fuck="wget -qO - rage.thewaffleshop.net"
alias grep="grep --color=always"
alias ls='ls --color=auto -hF --group-directories-first --time-style=+"%Y-%m-%d %H:%M"' 
alias l='ls -l'
alias la='ls -la'
alias lo=locate
alias m="cd /home/rolf/TheVault/musikk"
alias mv="mv -v"
alias nano="nano -w"
alias p1="vlc http://lyd.nrk.no/nrk_radio_p1_ostlandssendingen_mp3_h &"
alias p2="vlc http://lyd.nrk.no/nrk_radio_p2_mp3_h &"
alias pac='sudo pacman'
alias pico=nano
alias psg="ps aux | grep"
#alias rbcl="cat ~/bin/cmus/rebuild-library | cmus-remote"
alias rc="sudo rc.d"
#alias reload="source ~/.bashrc"
alias t=todo.sh
alias txa="tx | grep Down"
alias vl="/usr/share/vim/vim73/macros/less.sh"
alias x=startx

PATH=$PATH:/home/rolf/bin:.
export VMAIL_HTML_PART_READER='elinks -dump'

SSH_ENV="$HOME/.ssh/environment"

# start the ssh-agent
#function start_agent {
#    echo "Initializing new SSH agent..."
#    # spawn ssh-agent
#    ssh-agent | sed 's/^echo/#echo/' > "$SSH_ENV"
#    echo succeeded
#    chmod 600 "$SSH_ENV"
#    . "$SSH_ENV" > /dev/null
#    ssh-add
#}
#
## test for identities
#function test_identities {
#    # test whether standard identities have been added to the agent already
#    ssh-add -l | grep "The agent has no identities" > /dev/null
#    if [ $? -eq 0 ]; then
#        ssh-add
#        # $SSH_AUTH_SOCK broken so we start a new proper agent
#        if [ $? -eq 2 ];then
#            start_agent
#        fi
#    fi
#}
#
### check for running ssh-agent with proper $SSH_AGENT_PID
#if [ -n "$SSH_AGENT_PID" ]; then
#    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
#    if [ $? -eq 0 ]; then
#	test_identities
#    fi
### if $SSH_AGENT_PID is not properly set, we might be able to load one from
### $SSH_ENV
#else
#    if [ -f "$SSH_ENV" ]; then
#	. "$SSH_ENV" > /dev/null
#    fi
#    ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent > /dev/null
#    if [ $? -eq 0 ]; then
#        test_identities
#    else
#        start_agent
#    fi
#fi

# some utils
define() {
        echo
        curl -s dict://dict.org/d:$1 | grep -v '^[0-9]'
        echo
}

service() {
   sudo /etc/rc.d/$@
}

export LS_COLORS="bd=38;5;68:ca=38;5;17:cd=38;5;113;1:di=38;5;32:do=38;5;127:ex=38;5;148;1:pi=38;5;126:fi=38;5;253:ln=target:mh=38;5;220;1:no=38;5;254:or=48;5;196;38;5;232;1:ow=38;5;178;1:sg=38;5;137;1:su=38;5;137:so=38;5;197:st=38;5;208;1:tw=38;5;220;1:*.PL=38;5;160:*.asm=38;5;240;1:*.awk=38;5;148;1:*.bash=38;5;173:*.c=38;5;110:*.cfg=1:*.coffee=38;5;94;1:*.conf=1:*.cpp=38;5;24;1:*.cc=38;5;24;1:*.cs=38;5;74;1:*.css=38;5;91:*.csv=38;5;78:*.diff=48;5;197;38;5;232:*.enc=38;5;192;3:*.eps=38;5;192:*.etx=38;5;172:*.ex=38;5;148;1:*.example=38;5;225;1:*.git=38;5;197:*.gitignore=38;5;240:*.go=38;5;36;1:*.h=38;5;81:*.hs=38;5;159:*.htm=38;5;125;1:*.html=38;5;125;1:*.info=38;5;101:*.ini=38;5;122:*.java=38;5;142;1:*.jhtm=38;5;125;1:*.js=38;5;42:*.jsm=38;5;42:*.jsm=38;5;42:*.json=38;5;199:*.jsp=38;5;45:*.lisp=38;5;204;1:*.log=38;5;190:*.lua=38;5;34;1:*.map=38;5;58;3:*.markdown=38;5;184:*.md=38;5;184:*.mf=38;5;220;3:*.mfasl=38;5;73:*.mi=38;5;124:*.mkd=38;5;184:*.mtx=38;5;36;3:*.nfo=38;5;220:*.pacnew=38;5;33:*.patch=48;5;197;38;5;232:*.pc=38;5;100:*.pfa=38;5;43:*.php=38;5;93:*.pid=38;5;160:*.pl=38;5;214:*.pm=38;5;197;1:*.pod=38;5;172;1:*.py=38;5;41:*.rb=38;5;192:*.rdf=38;5;144:*.ru=38;5;142:*.sed=38;5;130;1:*.sfv=38;5;197:*.sh=38;5;113:*.signature=38;5;206:*.sty=38;5;58:*.sug=38;5;44:*.t=38;5;28;1:*.tcl=38;5;64;1:*.tdy=38;5;214:*.tex=38;5;172:*.textile=38;5;106:*.tfm=38;5;64:*.tfnt=38;5;140:*.theme=38;5;109:*.txt=38;5;214:*.urlview=38;5;85:*.vim=1:*.xml=38;5;199:*.yml=38;5;208:*.zsh=38;5;173:*.1=38;5;196;1:*.1p=38;5;160:*.3p=38;5;160:*.old=38;5;242:*.out=38;5;46;1:*.bmp=38;5;62:*.cdr=38;5;59:*.gif=38;5;72:*.ico=38;5;73:*.jpeg=38;5;124:*.jpg=38;5;124:*.JPG=38;5;124:*.png=38;5;72:*.svg=38;5;24;1:*.xpm=38;5;36:*.32x=38;5;137:*.A64=38;5;82:*.a00=38;5;11:*.a52=38;5;112:*.a64=38;5;82:*.a78=38;5;112:*.adf=38;5;35:*.atr=38;5;213:*.cdi=38;5;124:*.fm2=38;5;35:*.gb=38;5;203:*.gba=38;5;205:*.gbc=38;5;204:*.gel=38;5;83:*.gg=38;5;138:*.ggl=38;5;83:*.j64=38;5;102:*.nds=38;5;193:*.nes=38;5;160:*.rom=38;5;59;1:*.sav=38;5;220:*.sms=38;5;33:*.st=38;5;208;1:*.iso=38;5;124:*.nrg=38;5;124:*.qcow=38;5;141:*.MOV=38;5;42:*.avi=38;5;114:*.divx=38;5;107:*.flv=38;5;131:*.mkv=38;5;202:*.mov=38;5;42:*.mp4=38;5;124:*.mpg=38;5;38:*.mpeg=38;5;38:*.ogv=38;5;94:*.rmvb=38;5;112:*.sample=38;5;130;1:*.ts=38;5;39:*.wmv=38;5;113:*.S3M=38;5;71;1:*.cue=38;5;112:*.dat=38;5;165:*.fcm=38;5;41:*.flac=38;5;40;1:*.shn=38;5;28;1:*.m3u=38;5;172:*.m4=38;5;196;3:*.m4a=38;5;137;1:*.mod=38;5;72:*.mp3=38;5;191:*.s3m=38;5;71;1:*.sid=38;5;69;1:*.spl=38;5;173:*.afm=38;5;58:*.pfb=38;5;58:*.pfm=38;5;58:*.ttf=48;5;1:*.pcf=38;5;65:*.psf=38;5;64:*.bak=38;5;41;1:*.bin=38;5;249:*.swo=38;5;236:*.swp=38;5;241:*.tmp=38;5;244:*.un~=38;5;240:*.db=38;5;60:*.dump=38;5;119:*.sqlite=38;5;60:*.typelib=38;5;60:*.7z=38;5;40:*.a=38;5;46:*.arj=38;5;41:*.cbr=38;5;140:*.cbz=38;5;140:*.chm=38;5;144:*.jad=38;5;50:*.jar=38;5;51:*.part=38;5;240;1:*.pdf=38;5;202:*.abw=38;5;214:*.r00=38;5;233:*.r01=38;5;234:*.r02=38;5;235:*.r03=38;5;236:*.r04=38;5;237:*.r05=38;5;238:*.r06=38;5;239:*.r07=38;5;240:*.r08=38;5;241:*.r09=38;5;242:*.r10=38;5;243:*.r100=38;5;244:*.r101=38;5;240:*.r102=38;5;241:*.r103=38;5;242:*.r104=38;5;243:*.r105=38;5;244:*.r106=38;5;245:*.r107=38;5;246:*.r108=38;5;247:*.r109=38;5;248:*.r11=38;5;244:*.r110=38;5;249:*.r111=38;5;250:*.r112=38;5;251:*.r113=38;5;252:*.r114=38;5;253:*.r115=38;5;254:*.r116=38;5;255:*.r12=38;5;245:*.r13=38;5;246:*.r14=38;5;247:*.r15=38;5;248:*.r16=38;5;249:*.r17=38;5;250:*.r18=38;5;251:*.r19=38;5;252:*.r20=38;5;253:*.r21=38;5;254:*.r22=38;5;255:*.r25=38;5;255:*.r26=38;5;254:*.r27=38;5;253:*.r28=38;5;252:*.r29=38;5;251:*.r30=38;5;250:*.r31=38;5;249:*.r32=38;5;248:*.r33=38;5;247:*.r34=38;5;246:*.r35=38;5;245:*.r36=38;5;244:*.r37=38;5;243:*.r38=38;5;242:*.r39=38;5;241:*.r40=38;5;240:*.r41=38;5;239:*.r42=38;5;238:*.r43=38;5;237:*.r44=38;5;236:*.r45=38;5;235:*.r46=38;5;234:*.r47=38;5;233:*.r48=38;5;234:*.r49=38;5;235:*.r50=38;5;236:*.r51=38;5;237:*.r52=38;5;238:*.r53=38;5;239:*.r54=38;5;240:*.r55=38;5;241:*.r56=38;5;242:*.r57=38;5;243:*.r58=38;5;244:*.r59=38;5;245:*.r60=38;5;246:*.r61=38;5;247:*.r62=38;5;248:*.r63=38;5;249:*.r64=38;5;250:*.r65=38;5;251:*.r66=38;5;252:*.r67=38;5;253:*.r68=38;5;254:*.r69=38;5;255:*.r69=38;5;255:*.r70=38;5;254:*.r71=38;5;253:*.r72=38;5;252:*.r73=38;5;251:*.r74=38;5;250:*.r75=38;5;249:*.r76=38;5;248:*.r77=38;5;247:*.r78=38;5;246:*.r79=38;5;245:*.r80=38;5;244:*.r81=38;5;243:*.r82=38;5;242:*.r83=38;5;241:*.r84=38;5;240:*.r85=38;5;239:*.r86=38;5;238:*.r87=38;5;237:*.r88=38;5;236:*.r89=38;5;235:*.r90=38;5;234:*.r91=38;5;235:*.r92=38;5;236:*.r93=38;5;237:*.r94=38;5;238:*.r95=38;5;239:*.r96=38;5;240:*.r97=38;5;241:*.r98=38;5;242:*.r99=38;5;243:*.rar=38;5;106;1:*.tar=38;5;118:*.tar.gz=38;5;34:*.tgz=38;5;34:*.xz=38;5;118:*.zip=38;5;41:*.bz2=38;5;35;1:*.SKIP=38;5;244:*.def=38;5;136:*.directory=38;5;83:*.err=38;5;160;1:*.error=38;5;160;1:*.pi=38;5;126:*.properties=38;5;197;1:*.torrent=38;5;58:*.md5=38;5;202:*.par2=38;5;114:*.ape=38;5;123:*.ogg=38;5;192"
source ~/.oh-my-zsh/zsh-history-substring-search.zsh

echo
fortune
echo
