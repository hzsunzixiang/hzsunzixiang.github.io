# Zixiang Sun's Personal Blog

Welcome to my personal blog built with Jekyll and featuring a custom Indigo theme! This is where I share my thoughts on technology, programming, and life experiences.

## 🌟 Features

- **Custom Indigo Theme**: Beautiful Material Design-inspired interface
- **Responsive Design**: Perfect on desktop, tablet, and mobile devices
- **Fast Performance**: Optimized for speed and SEO
- **Modern Jekyll**: Built with Jekyll 4.x and latest best practices
- **GitHub Pages Ready**: Automatic deployment via GitHub Actions

## 🚀 Live Site

Visit the live site at: [https://hzsunzixiang.github.io](https://hzsunzixiang.github.io)

## 🛠️ Local Development

### Prerequisites

- Ruby 2.5.0 or higher
- Bundler gem

### Setup

1. Clone the repository:
```bash
git clone https://github.com/hzsunzixiang/hzsunzixiang.github.io.git
cd hzsunzixiang.github.io
```

2. Install dependencies:
```bash
bundle install
```

3. Serve the site locally:
```bash
bundle exec jekyll serve
```

4. Open your browser and visit `http://localhost:4000`

### Development Commands

```bash
# Clean build files
bundle exec jekyll clean

# Build the site
bundle exec jekyll build

# Serve with live reload
bundle exec jekyll serve --livereload

# Serve with drafts
bundle exec jekyll serve --drafts
```

## 📝 Writing Posts

Create new posts in the `_posts` directory with the following naming convention:
```
YYYY-MM-DD-title-of-post.md
```

Each post should start with front matter:
```yaml
---
layout: post
title: "Your Post Title"
date: 2024-01-15 10:00:00 +0800
categories: [category1, category2]
tags: [tag1, tag2, tag3]
author: Zixiang Sun
---
```

## 🎨 Theme Customization

The Indigo theme can be customized by modifying:

- **Colors**: Edit CSS variables in `assets/css/indigo.css`
- **Layouts**: Modify files in `_layouts/` directory
- **Includes**: Edit reusable components in `_includes/` directory
- **Configuration**: Update `_config.yml` for site settings

## 📁 Project Structure

```
├── _layouts/           # Page layouts
├── _posts/            # Blog posts
├── _includes/         # Reusable components
├── assets/
│   ├── css/          # Stylesheets
│   ├── js/           # JavaScript files
│   └── images/       # Images and media
├── _config.yml       # Jekyll configuration
├── index.html        # Homepage
├── about.md          # About page
├── archive.html      # Archive page
└── categories.html   # Categories page
```

## 🚀 Deployment

This site is automatically deployed to GitHub Pages using GitHub Actions. Every push to the `main` branch triggers a new build and deployment.

### Manual Deployment

If you prefer manual deployment:

1. Build the site:
```bash
bundle exec jekyll build
```

2. The generated site will be in the `_site` directory

## 🤝 Contributing

Feel free to:
- Report bugs or issues
- Suggest new features
- Submit pull requests
- Share feedback

## 📄 License

This project is open source and available under the [MIT License](LICENSE).

## 📞 Contact

- **Email**: [17842379@qq.com](mailto:17842379@qq.com)
- **GitHub**: [@hzsunzixiang](https://github.com/hzsunzixiang)
- **Website**: [https://hzsunzixiang.github.io](https://hzsunzixiang.github.io)

---

Built with ❤️ using Jekyll and the custom Indigo theme.
github blog
