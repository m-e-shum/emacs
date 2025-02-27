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

Therefore, to add your own customizations, use the following files:
* `pre-init.el`
* `post-init.el`
* `pre-early-init.el`
* `post-early-init.el`
