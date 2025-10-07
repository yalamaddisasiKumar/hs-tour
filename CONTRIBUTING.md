# Contributing to Haskell Design Patterns Guide

Thank you for your interest in contributing! This guide aims to be a comprehensive resource for the Haskell community, and we welcome contributions of all kinds.

## 🎯 How You Can Contribute

### Content Contributions
- Fix typos or grammatical errors
- Improve code examples
- Add missing patterns
- Enhance explanations
- Add more use cases
- Update outdated information

### Technical Contributions
- Improve styling and design
- Enhance navigation
- Add new features
- Fix bugs
- Improve performance

### Documentation
- Improve README
- Add more examples
- Create tutorials
- Translate content

## 📝 Content Guidelines

### Pattern Page Structure

Each pattern page should follow this structure:

```markdown
---
title: Pattern Name
---

## Overview
Brief introduction (2-3 paragraphs)

## Core Concepts
- Key concept 1
- Key concept 2
- Key concept 3

## Type Signature
```haskell
-- Essential type definitions
```

## Examples
### Basic Example
[Working code with explanation]

### Advanced Example
[More complex example]

## Common Use Cases
1. Use case 1
2. Use case 2
3. Use case 3

## Related Patterns
- [Link to related pattern 1]
- [Link to related pattern 2]

## Best Practices
1. Practice 1
2. Practice 2

## Further Reading
- External resource 1
- External resource 2
```

### Code Style

**Haskell Code:**
- Use clear, descriptive names
- Add type signatures
- Include comments for complex logic
- Follow standard Haskell style (2-space indentation)
- Ensure code compiles and runs

**Example:**
```haskell
-- Good
calculateAverage :: [Double] -> Double
calculateAverage xs = sum xs / fromIntegral (length xs)

-- Not as good (missing type signature and unclear name)
calc xs = sum xs / fromIntegral (length xs)
```

### Writing Style

- **Be clear and concise**: Avoid unnecessary jargon
- **Use examples**: Show, don't just tell
- **Be accurate**: Verify technical details
- **Link related concepts**: Connect to other patterns
- **Cite sources**: Reference papers, blog posts, libraries

## 🔧 Development Setup

### Prerequisites
- Git
- Ruby 2.7 or higher
- Bundler

### Setup Steps

1. **Fork and clone:**
   ```bash
   git clone https://github.com/yalamaddisasiKumar/hs-tour.git
   cd hs-tour
   ```

2. **Install dependencies:**
   ```bash
   bundle install
   ```

3. **Run local server:**
   ```bash
   bundle exec jekyll serve
   ```

4. **View at:** `http://localhost:4000/hs-tour/`

## 📤 Submission Process

### For Small Changes (typos, small fixes)

1. Fork the repository
2. Make your changes
3. Submit a pull request

### For Larger Changes (new patterns, major updates)

1. **Open an issue first** to discuss the change
2. Wait for feedback
3. Fork and create a feature branch
4. Make your changes
5. Test locally
6. Submit a pull request

### Pull Request Guidelines

**Good PR Title Examples:**
- "Fix: Correct type signature in Functor example"
- "Add: New pattern - Continuation Monad"
- "Improve: Enhanced explanation for Applicative laws"

**PR Description Should Include:**
- What changes were made
- Why the changes were needed
- Any related issues
- Screenshots (for UI changes)

**Before Submitting:**
- [ ] Test locally with `jekyll serve`
- [ ] Check for typos and grammar
- [ ] Ensure code examples work
- [ ] Follow existing style
- [ ] Update relevant documentation

## 🎨 Style Guide

### Markdown

```markdown
# H1 - Only for page title
## H2 - Major sections
### H3 - Subsections
#### H4 - Minor subsections

**Bold** for emphasis
*Italic* for terms
`code` for inline code
```

### Code Blocks

```markdown
```haskell
-- Always specify language for syntax highlighting
function :: Type
function = implementation
`` `
```

### Links

```markdown
[Link text](url) - External links
[Pattern Name](../path/pattern) - Internal links
```

## 🐛 Reporting Issues

When reporting issues, include:
- **Title**: Clear, descriptive title
- **Description**: What's wrong?
- **Steps to reproduce**: How to see the issue
- **Expected behavior**: What should happen?
- **Screenshots**: If applicable
- **Environment**: Browser, OS, Jekyll version

## ✅ Review Process

1. **Submission**: You submit a PR
2. **Initial Review**: Maintainer reviews within 1 week
3. **Feedback**: You may receive change requests
4. **Revisions**: Make requested changes
5. **Approval**: Once approved, PR is merged
6. **Deploy**: Changes go live automatically

## 🏆 Recognition

Contributors will be:
- Listed in a CONTRIBUTORS.md file (coming soon)
- Credited in commit messages
- Acknowledged in release notes

## 📋 Checklist for New Patterns

When adding a new pattern:

- [ ] Create `.md` file in `_basic/` or `_advanced/`
- [ ] Add front matter with title
- [ ] Include all standard sections
- [ ] Provide working code examples
- [ ] Add to `index.md` navigation
- [ ] Link related patterns
- [ ] Include external resources
- [ ] Test locally
- [ ] Submit PR with clear description

## 🎓 Resources for Contributors

### Haskell Resources
- [Haskell.org](https://www.haskell.org/)
- [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
- [Learn You a Haskell](http://learnyouahaskell.com/)

### Jekyll Resources
- [Jekyll Documentation](https://jekyllrb.com/docs/)
- [Markdown Guide](https://www.markdownguide.org/)
- [Liquid Templates](https://shopify.github.io/liquid/)

### Git Resources
- [Git Book](https://git-scm.com/book/en/v2)
- [GitHub Flow](https://guides.github.com/introduction/flow/)

## ❓ Questions?

- Check existing issues
- Read the README.md
- Ask in discussions
- Open a new issue

## 📜 Code of Conduct

### Our Pledge

We are committed to providing a welcoming and inspiring community for all.

### Our Standards

**Positive behavior:**
- Be respectful and inclusive
- Accept constructive criticism
- Focus on what's best for the community
- Show empathy

**Unacceptable behavior:**
- Harassment or discrimination
- Trolling or insulting comments
- Personal or political attacks
- Publishing others' private information

### Enforcement

Report unacceptable behavior to the project maintainers. All complaints will be reviewed and investigated.

## 📞 Contact

- **Issues**: GitHub Issues
- **Discussions**: GitHub Discussions
- **Email**: (Add your email if desired)

## 🙏 Thank You

Your contributions help make this guide better for everyone learning Haskell. Whether you're fixing a typo or adding a new pattern, every contribution is valuable!

---

**Happy Contributing!** 🎉
