# Quick Start Guide

## 🚀 Get Started in 5 Minutes

### For GitHub Pages (No Installation Required)

1. **Fork this repository** to your GitHub account

2. **Enable GitHub Pages:**
   - Go to your repository's Settings
   - Navigate to "Pages" in the left sidebar
   - Under "Source", select the `main` branch
   - Click "Save"

3. **Wait a moment** for GitHub to build your site (usually 1-2 minutes)

4. **Visit your site** at: `https://yalamaddisasiKumar.github.io/hs-tour/`

That's it! Your Haskell Design Patterns Guide is now live.

### For Local Development

If you want to preview changes locally before pushing:

1. **Install Ruby** (if not already installed):
   ```bash
   # Windows (using Chocolatey)
   choco install ruby
   
   # macOS
   brew install ruby
   
   # Linux (Ubuntu/Debian)
   sudo apt-get install ruby-full build-essential
   ```

2. **Install dependencies:**
   ```bash
   cd hs-tour
   gem install bundler
   bundle install
   ```

3. **Run the local server:**
   ```bash
   bundle exec jekyll serve
   ```

4. **Open your browser** to: `http://localhost:4000/hs-tour/`

### Making Changes

1. **Edit content** - All pattern pages are in `_basic/` and `_advanced/` folders
2. **Save your changes**
3. **Commit and push** to GitHub
4. **GitHub Pages automatically rebuilds** your site

### Customization

**Change the site title and description:**
Edit `_config.yml`:
```yaml
title: My Haskell Guide
description: My custom description
```

**Change colors and styling:**
Edit `assets/css/style.css`

**Add new patterns:**
1. Create a new `.md` file in `_basic/` or `_advanced/`
2. Add front matter:
   ```yaml
   ---
   title: New Pattern
   ---
   ```
3. Write your content
4. Add link in `index.md`

## 📚 Navigation

- **Home**: Overview and pattern list
- **Basic Patterns**: Fundamental Haskell patterns
- **Advanced Patterns**: Advanced techniques and libraries

## 🆘 Troubleshooting

**Site not showing up?**
- Check Settings → Pages to ensure it's enabled
- Wait a few minutes for the build to complete
- Check the Actions tab for any build errors

**Local server won't start?**
- Ensure Ruby is installed: `ruby --version`
- Reinstall dependencies: `bundle install`
- Check for port conflicts: try `jekyll serve --port 4001`

**Styling looks broken?**
- Clear your browser cache
- Check that `assets/css/style.css` exists
- Verify the `_layouts/` folder has both template files

## 💡 Tips

- Use Markdown for all content
- Code blocks support syntax highlighting
- Images can be placed in `assets/images/`
- Keep pattern pages focused and well-structured

## 🔗 Next Steps

1. Read through the Basic Patterns
2. Try the Advanced Patterns when comfortable
3. Customize the guide for your needs
4. Share with others learning Haskell!

---

**Need help?** Open an issue on GitHub or check the main README.md
