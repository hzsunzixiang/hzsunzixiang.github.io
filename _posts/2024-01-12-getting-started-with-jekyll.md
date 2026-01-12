---
layout: post
title: "Getting Started with Jekyll: A Developer's Guide"
date: 2024-01-12 14:30:00 +0800
categories: [tutorial, web-development]
tags: [jekyll, static-site, github-pages, ruby]
author: Zixiang Sun
---

# Getting Started with Jekyll: A Developer's Guide

Jekyll is a fantastic static site generator that's perfect for creating blogs, documentation sites, and personal websites. In this comprehensive guide, I'll walk you through everything you need to know to get started with Jekyll.

## What is Jekyll?

Jekyll is a static site generator written in Ruby. It takes text written in your favorite markup language and uses layouts to create a static website. Jekyll is the engine behind GitHub Pages, which means you can host your Jekyll-powered website for free on GitHub.

### Key Features

- **Simple**: No more databases, comment moderation, or pesky updates to install
- **Static**: Markdown, Liquid, HTML & CSS go in. Static sites come out ready for deployment
- **Blog-aware**: Permalinks, categories, pages, posts, and custom layouts are all first-class citizens

## Installation

### Prerequisites

Before installing Jekyll, make sure you have:

- Ruby version 2.5.0 or higher
- RubyGems
- GCC and Make

### Installing Jekyll

```bash
# Install Jekyll and bundler gems
gem install jekyll bundler

# Create a new Jekyll site
jekyll new my-awesome-site

# Change into your new directory
cd my-awesome-site

# Build the site and serve it locally
bundle exec jekyll serve
```

Your site will be available at `http://localhost:4000`.

## Directory Structure

A basic Jekyll site usually looks something like this:

```
.
├── _config.yml
├── _data
│   └── members.yml
├── _drafts
│   ├── begin-with-the-crazy-ideas.md
│   └── on-simplicity-in-technology.md
├── _includes
│   ├── footer.html
│   └── header.html
├── _layouts
│   ├── default.html
│   └── post.html
├── _posts
│   ├── 2007-10-29-why-every-programmer-should-play-nethack.md
│   └── 2009-04-26-barcamp-boston-4-roundup.md
├── _sass
│   ├── _base.scss
│   └── _layout.scss
├── _site
├── .jekyll-metadata
└── index.html # can also be an 'index.md' with valid front matter
```

## Configuration

Jekyll's configuration is stored in the `_config.yml` file. Here's a basic configuration:

```yaml
title: Your awesome title
email: your-email@example.com
description: >-
  Write an awesome description for your new site here.
baseurl: ""
url: "http://yourdomain.com"

# Build settings
markdown: kramdown
highlighter: rouge
theme: minima
plugins:
  - jekyll-feed
  - jekyll-sitemap
  - jekyll-seo-tag

# Exclude from processing
exclude:
  - Gemfile
  - Gemfile.lock
  - node_modules
  - vendor/
```

## Writing Posts

Posts are stored in the `_posts` directory and follow a specific naming convention:

```
YEAR-MONTH-DAY-title.MARKUP
```

For example: `2024-01-12-getting-started-with-jekyll.md`

### Front Matter

Every post must begin with front matter:

```yaml
---
layout: post
title: "Getting Started with Jekyll"
date: 2024-01-12 14:30:00 +0800
categories: [tutorial, web-development]
tags: [jekyll, static-site, github-pages]
author: Your Name
---
```

## Liquid Templating

Jekyll uses the Liquid templating language to process templates. Here are some common Liquid tags:

### Variables

```liquid
{{ site.title }}
{{ page.title }}
{{ content }}
```

### Loops

```liquid
{% for post in site.posts %}
  <h3>{{ post.title }}</h3>
  <p>{{ post.excerpt }}</p>
{% endfor %}
```

### Conditionals

```liquid
{% if page.title %}
  <h1>{{ page.title }}</h1>
{% endif %}
```

## Layouts

Layouts are templates that wrap around your content. They're stored in the `_layouts` directory.

### Default Layout Example

```html
<!DOCTYPE html>
<html>
<head>
  <title>{{ page.title }} | {{ site.title }}</title>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
</head>
<body>
  <header>
    <h1>{{ site.title }}</h1>
  </header>
  
  <main>
    {{ content }}
  </main>
  
  <footer>
    <p>&copy; {{ 'now' | date: "%Y" }} {{ site.title }}</p>
  </footer>
</body>
</html>
```

## Deployment

### GitHub Pages

The easiest way to deploy a Jekyll site is using GitHub Pages:

1. Create a repository named `username.github.io`
2. Push your Jekyll site to the repository
3. Your site will be available at `https://username.github.io`

### Custom Deployment

For custom deployment, build your site and upload the `_site` directory:

```bash
# Build the site
bundle exec jekyll build

# The generated site is now in _site/
```

## Tips and Best Practices

### 1. Use Collections for Organized Content

```yaml
# _config.yml
collections:
  projects:
    output: true
    permalink: /:collection/:name/
```

### 2. Optimize Images

Use responsive images and optimize file sizes:

```html
<img src="{{ '/assets/images/photo.jpg' | relative_url }}" 
     alt="Description" 
     loading="lazy">
```

### 3. Use Data Files

Store repetitive data in YAML files in the `_data` directory:

```yaml
# _data/navigation.yml
- name: Home
  link: /
- name: About
  link: /about/
- name: Blog
  link: /blog/
```

### 4. Performance Optimization

- Minify CSS and JavaScript
- Use Jekyll plugins for SEO
- Implement lazy loading for images
- Use a CDN for assets

## Common Issues and Solutions

### Issue: Site not updating locally

**Solution**: Clear Jekyll cache and rebuild:

```bash
bundle exec jekyll clean
bundle exec jekyll serve
```

### Issue: Plugin not working on GitHub Pages

**Solution**: Check if the plugin is supported by GitHub Pages or use GitHub Actions for custom builds.

### Issue: Slow build times

**Solution**: 
- Use `--incremental` flag for faster rebuilds
- Exclude unnecessary files in `_config.yml`
- Optimize large collections

## Conclusion

Jekyll is a powerful and flexible static site generator that's perfect for developers who want full control over their website. With its simple setup, powerful templating system, and excellent GitHub integration, it's an ideal choice for blogs, portfolios, and documentation sites.

The learning curve might seem steep at first, but once you understand the basics of layouts, includes, and Liquid templating, you'll be able to create sophisticated websites with ease.

## Resources

- [Jekyll Official Documentation](https://jekyllrb.com/docs/)
- [Liquid Template Language](https://shopify.github.io/liquid/)
- [Jekyll Themes](https://jekyllthemes.io/)
- [GitHub Pages Documentation](https://docs.github.com/en/pages)

Happy Jekyll-ing! 🚀