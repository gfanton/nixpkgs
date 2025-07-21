# Modern fzf configuration for enhanced command-line experience

# Core fzf configuration
export FZF_DEFAULT_OPTS='
  --height=40% 
  --layout=reverse 
  --border=rounded
  --margin=1 
  --padding=1
  --info=inline
  --prompt="❯ " 
  --pointer="▶" 
  --marker="✓"
  --color=fg:#908caa,bg:#191724,hl:#ebbcba
  --color=fg+:#e0def4,bg+:#26233a,hl+:#f6c177
  --color=border:#403d52,header:#31748f,gutter:#191724
  --color=spinner:#f6c177,info:#9ccfd8,separator:#403d52
  --color=pointer:#c4a7e7,marker:#eb6f92,prompt:#908caa
  --bind=tab:accept,shift-tab:up,ctrl-j:down,ctrl-k:up
  --bind=ctrl-u:half-page-up,ctrl-d:half-page-down
  --bind=ctrl-space:toggle,ctrl-a:select-all,ctrl-alt-a:deselect-all
  --cycle'

# Use fd for file search (faster than find)
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

# CTRL-T: Fuzzy file finder
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS='
  --preview "bat --color=always --style=numbers --line-range=:500 {}"
  --preview-window=right:60%:wrap
  --bind "ctrl-y:execute-silent(echo {} | pbcopy)"
  --header "CTRL-Y: copy path to clipboard"'

# ALT-C: Fuzzy directory navigation
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'
export FZF_ALT_C_OPTS='
  --preview "eza --tree --level=2 --color=always {}"
  --preview-window=right:60%:wrap'

# CTRL-R: Enhanced history search
export FZF_CTRL_R_OPTS='
  --preview "echo {}" 
  --preview-window=down:3:wrap
  --bind "ctrl-y:execute-silent(echo {2..} | pbcopy)"
  --header "CTRL-Y: copy command to clipboard"
  --exact'

# Custom fzf functions for enhanced workflow

# fzf-powered file finder with preview
ff() {
  local file
  file=$(fd --type f --hidden --follow --exclude .git | fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}') && $EDITOR "$file"
}

# fzf-powered directory navigator
fd_nav() {
  local dir
  dir=$(fd --type d --hidden --follow --exclude .git | fzf --preview 'eza --tree --level=2 --color=always {}') && cd "$dir"
}

# fzf git branch switcher
fgb() {
  local branch
  branch=$(git branch --all | grep -v HEAD | sed 's/.* //' | sed 's#remotes/[^/]*/##' | sort -u | fzf --preview 'git log --oneline --graph --color=always {} | head -20') && git checkout "$branch"
}

# fzf git log browser
fgl() {
  git log --graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" | fzf --ansi --no-sort --reverse --tiebreak=index --preview 'echo {} | grep -o "[a-f0-9]\{7\}" | head -1 | xargs git show --color=always' --bind 'enter:execute(echo {} | grep -o "[a-f0-9]\{7\}" | head -1 | xargs git show --color=always | less -R)'
}

# fzf process killer
fkill() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m --preview 'echo {}' --preview-window=down:3:wrap | awk '{print $2}')
  if [ "x$pid" != "x" ]; then
    echo $pid | xargs kill -${1:-9}
  fi
}

# fzf environment variable browser
fenv() {
  env | fzf --preview 'echo {}' --preview-window=down:3:wrap
}

# Key bindings for better integration
bindkey '^T' fzf-file-widget      # CTRL-T: file finder
bindkey '^R' fzf-history-widget   # CTRL-R: history search
bindkey '\ec' fzf-cd-widget       # ALT-C: cd widget
