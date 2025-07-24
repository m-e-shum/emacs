# README: My EMACS Configuration

## Use `minimal-emacs.d` for better defaults and optimized start up

My previous config was too complicated for me to maintain. I am hoping this minimal setup will help:
1. Simplify
2. Reduce maintenance burden

[Here's a link to `minimal-emacs.d`](https://github.com/jamescherti/minimal-emacs.d/tree/main?tab=readme-ov-file#install-minimal-emacsd)
```bash
git clone https://github.com/jamescherti/minimal-emacs.d ~/.emacs.d
```

## Soft link to `*init.el`

`minimal-emacs.d` requires for you to NEVER edit the following files:
* `early-init.el`
* `init.el`

```bash
ln -s minimal-emacs.d/early-init.el
ln -s minimal-emacs.d/init.el
```

## Use `org-babel-tangle` to create custom files

Tangle `init.org` to create the following files:
* `pre-init.el`
* `post-init.el`
* `pre-early-init.el`
* `post-early-init.el`

## Use emacsclient

Put below in your `~/.bashrc`:

```bash
function e {
    local file="$1"
    emacsclient -nw --socket-name=shumma1 "$file" || (emacs --daemon=shumma1 && sleep 1 && emacsclient -nw --socket-name=shumma1 "$file")
}
export e
# to use, type: e <file_name>
```
