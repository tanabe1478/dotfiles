. /opt/homebrew/opt/asdf/libexec/asdf.sh
export PATH="$PATH://Users/tanabe.nobuyuki/.asdf/shims/flutter"
export WORK_DIR=~/workspace
export BARISTA_DIR=$WORK_DIR/barista
export ECR_URL=191736117997.dkr.ecr.ap-northeast-1.amazonaws.com/jp.prismatix.aws.metropolis/techdoc-ja-mkdocs
export TECHDOC_JA_MKDOCS=4.1.0
export CMAPI_LINT_FOR_DOC_LEVEL=light
export CMAPI_LINT_FOR_DOC_CONTENT_DIR=/root/workspace/barista/docs/src/apispec/content
export PATH="/Users/tanabe.nobuyuki/.asdf/installs/python/3.8.17/bin:$PATH"

# DeploygateのAPIキー
export DEPLOYGATE_DEV_API_KEY="09e13fb9-95b2-473d-9579-e8a9b74f6f0e"

# colorize prompt
# わかりにくいので削除
# autoload -Uz colors && colors
# PROMPT="%F{green}%n%f %F{cyan}($(arch))%f:%F{blue}%~%f"$'\n'"%# "

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/tanabe.nobuyuki/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

# コマンド実行結果の後に空行を挿入する
add_newline() {
  if [[ -z $PS1_NEWLINE_LOGIN ]]; then
    PS1_NEWLINE_LOGIN=true
  else
    printf '\n'
  fi
}

# コマンド実行前処理
precmd() {
  add_newline
}

# インクリメンタルなgit checkout
gcop() {
  git branch -a --sort=-authordate |
    grep -v -e '->' -e '*' |
    perl -pe 's/^\h+//g' |
    perl -pe 's#^remotes/origin/##' |
    perl -nle 'print if !$c{$_}++' |
    peco |
    xargs git checkout
}

# PRをpecoで選択してブラウザで開く
if type gh > /dev/null 2>&1; then
  function gh-pr-list-peco() {
    PR_URL=$(gh pr list --json number,title,url | jq -r '.[] | [.number, .title, .url] | @tsv' | peco | awk '{print $3}')
    gh pr view $PR_URL -w
  }
  alias prlw=gh-pr-list-peco
fi



# show pretty commit log
if type git > /dev/null 2>&1; then
  function git-log-one-liner() {
    git log --pretty='format:%C(yellow)%h %C(green)%cd %C(reset)%s %C(red)%d %C(cyan)[%an]' --date=format:'%c' --all --graph
  }

  alias glone=git-log-one-liner

  function git-push-origin-first() {
    branch_name=$(git symbolic-ref --short HEAD)
    git push --set-upstream origin $branch_name
  }

  alias first-origin-push=git-push-origin-first
fi

# batでzshrcを見る
if type bat > /dev/null 2>&1; then
  function zshrc() {
    bat ~/.zshrc
  }
fi

# zsh plugins
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
  source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
  autoload -Uz compinit
  compinit
fi

# ---------------------------------
# barista commands

barista_redpen_check() {
  current_dir=$(pwd)
  target_dir="/Users/tanabe.nobuyuki/workspace/barista"
  if [ "$current_dir" != "$target_dir" ]; then
    echo "this command can not be execute in thi directory. please exec this in ~/workspace/barista/"
  fi

  if [ -z "$MD_FILE" ]; then
    echo "MD_FILE is not defined"
    return 1
  fi
  docker run --rm -it -v ${WORK_DIR}:/root/workspace --workdir /root/workspace ${ECR_URL}:${TECHDOC_JA_MKDOCS} \
  sh -c "find ${MD_FILE} -type f | xargs redpen -l 0 -L ja -f markdown \
  -c /root/workspace/techdoc-ja-mkdocs/config/${CMAPI_LINT_FOR_DOC_LEVEL}/redpen-conf.xml"
}

barista_textlint_check() {
  current_dir=$(pwd)
  target_dir="/Users/tanabe.nobuyuki/workspace/barista"
  if [ "$current_dir" != "$target_dir" ]; then
    echo "this command can not be execute in thi directory. please exec this in ~/workspace/barista/"
  fi

  if [ -z "$MD_FILE" ]; then
    echo "MD_FILE is not defined"
    return 1
  fi
  docker run --rm -it -v ${WORK_DIR}:/root/workspace --workdir /root/workspace ${ECR_URL}:${TECHDOC_JA_MKDOCS} \
         textlint --format unix --config /root/workspace/techdoc-ja-mkdocs/config/${CMAPI_LINT_FOR_DOC_LEVEL}/textlint.json \
         ${MD_FILE} | sed -e 's/^\/root\/workspace\///'
}

barista_markdownlint_check() {
  current_dir=$(pwd)
  target_dir="/Users/tanabe.nobuyuki/workspace/barista"
  if [ "$current_dir" != "$target_dir" ]; then
    echo "this command can not be execute in thi directory. please exec this in ~/workspace/barista/"
  fi

  if [ -z "$MD_FILE" ]; then
    echo "MD_FILE is not defined"
    return 1
  fi
  docker run --rm -it -v ${WORK_DIR}:/root/workspace --workdir /root/workspace ${ECR_URL}:${TECHDOC_JA_MKDOCS} \
    markdownlint --config /root/workspace/techdoc-ja-mkdocs/config/${CMAPI_LINT_FOR_DOC_LEVEL}/markdownlint.json \
    ${MD_FILE} | sed -e "s/\.md: /\.md:/"
}
# ---------------------------------R

[ -f "/Users/tanabe.nobuyuki/.ghcup/env" ] && source "/Users/tanabe.nobuyuki/.ghcup/env" # ghcup-env

eval "$(zoxide init zsh --hook prompt )"



