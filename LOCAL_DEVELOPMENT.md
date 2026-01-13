# 🚀 Jekyll 博客本地开发指南

本指南提供了多种方法来在本地启动和调试 Jekyll 博客。

## 📋 快速启动

### 方法一：使用项目启动脚本（推荐）

```bash
# 给脚本执行权限（只需执行一次）
chmod +x local-dev.sh

# 启动开发服务器
./local-dev.sh
```

这个脚本会自动检测并使用最佳的启动方法。

## 🛠️ 详细启动方法

### 方法二：直接使用 Jekyll

#### 1. 安装依赖（首次运行）

```bash
# 安装 Ruby 和 Bundler（如果没有）
# macOS 用户推荐使用 Homebrew
brew install ruby
gem install bundler jekyll

# 安装项目依赖
bundle install
```

#### 2. 启动开发服务器

```bash
# 基本启动
bundle exec jekyll serve

# 带实时重载的启动（推荐）
bundle exec jekyll serve --livereload

# 指定主机和端口
bundle exec jekyll serve --host 0.0.0.0 --port 4000 --livereload

# 增量构建（更快）
bundle exec jekyll serve --livereload --incremental
```

### 方法三：使用 Docker（无需安装 Ruby）

#### 1. 使用 Docker Compose（推荐）

```bash
# 启动服务
docker-compose up

# 后台运行
docker-compose up -d

# 停止服务
docker-compose down
```

#### 2. 直接使用 Docker

```bash
# 一次性运行
docker run --rm -it \
  -p 4000:4000 \
  -p 35729:35729 \
  -v "$(pwd):/srv/jekyll" \
  jekyll/jekyll:4.2.2 \
  jekyll serve --watch --force_polling --host 0.0.0.0 --livereload
```

### 方法四：简单 HTTP 服务器（仅用于静态预览）

如果只是想快速预览静态文件（不处理 Jekyll 模板）：

```bash
# Python 3
python3 -m http.server 8080

# Python 2
python -m SimpleHTTPServer 8080

# Node.js
npx http-server -p 8080 -o

# PHP
php -S localhost:8080
```

## 🌐 访问地址

启动成功后，通过以下地址访问：

- **本地访问**: http://localhost:4000
- **局域网访问**: http://你的IP地址:4000
- **LiveReload**: 自动刷新（端口 35729）

## 🔧 常用开发命令

### Jekyll 命令

```bash
# 构建网站（生成 _site 目录）
bundle exec jekyll build

# 清理构建文件
bundle exec jekyll clean

# 检查配置
bundle exec jekyll doctor

# 创建新文章
bundle exec jekyll post "文章标题"

# 创建新页面
bundle exec jekyll page "页面名称"
```

### 依赖管理

```bash
# 更新依赖
bundle update

# 检查依赖
bundle outdated

# 安装新 gem
bundle add gem名称

# 移除 gem
bundle remove gem名称
```

## 📱 开发技巧

### 1. 实时预览

使用 `--livereload` 参数，当文件改变时浏览器会自动刷新：

```bash
bundle exec jekyll serve --livereload
```

### 2. 草稿预览

查看 `_drafts` 目录中的草稿文章：

```bash
bundle exec jekyll serve --drafts
```

### 3. 未来文章预览

预览发布日期在未来的文章：

```bash
bundle exec jekyll serve --future
```

### 4. 增量构建

只重新构建修改的文件（更快）：

```bash
bundle exec jekyll serve --incremental
```

### 5. 详细输出

查看详细的构建信息：

```bash
bundle exec jekyll serve --verbose
```

## 🐛 常见问题解决

### 问题1: Ruby 版本不兼容

```bash
# 使用 rbenv 管理 Ruby 版本
brew install rbenv
rbenv install 3.0.0
rbenv global 3.0.0

# 或使用 RVM
curl -sSL https://get.rvm.io | bash
rvm install 3.0.0
rvm use 3.0.0 --default
```

### 问题2: 依赖安装失败

```bash
# 清理并重新安装
bundle clean --force
rm Gemfile.lock
bundle install
```

### 问题3: 端口被占用

```bash
# 使用不同端口
bundle exec jekyll serve --port 4001

# 或者杀死占用端口的进程
lsof -ti:4000 | xargs kill -9
```

### 问题4: 权限问题

```bash
# 使用用户级别的 gem 安装
gem install --user-install bundler jekyll

# 或者使用 sudo（不推荐）
sudo gem install bundler jekyll
```

## 📊 性能优化

### 1. 排除不必要的文件

在 `_config.yml` 中添加：

```yaml
exclude:
  - node_modules
  - .git
  - .gitignore
  - README.md
  - Gemfile
  - Gemfile.lock
```

### 2. 使用增量构建

```bash
bundle exec jekyll serve --incremental --livereload
```

### 3. 限制文章数量（开发时）

```yaml
# _config.yml
limit_posts: 5  # 只构建最新的5篇文章
```

## 🔄 自动化脚本

### 创建新文章脚本

创建 `new-post.sh`：

```bash
#!/bin/bash
title="$1"
date=$(date +%Y-%m-%d)
filename="_posts/${date}-$(echo $title | tr '[:upper:]' '[:lower:]' | sed 's/ /-/g').md"

cat > "$filename" << EOF
---
layout: post
title: "$title"
date: $(date +"%Y-%m-%d %H:%M:%S %z")
categories: [blog]
tags: []
author: Zixiang Sun
---

# $title

内容开始...
EOF

echo "✅ 新文章已创建: $filename"
```

使用方法：
```bash
chmod +x new-post.sh
./new-post.sh "我的新文章标题"
```

## 📚 相关资源

- [Jekyll 官方文档](https://jekyllrb.com/docs/)
- [GitHub Pages 文档](https://docs.github.com/en/pages)
- [Liquid 模板语言](https://shopify.github.io/liquid/)
- [Markdown 语法](https://www.markdownguide.org/)

---

## 🎯 推荐工作流程

1. **启动开发服务器**: `./local-dev.sh`
2. **创建新文章**: 在 `_posts/` 目录下创建 `.md` 文件
3. **实时预览**: 浏览器访问 http://localhost:4000
4. **提交更改**: `git add . && git commit -m "描述" && git push`
5. **自动部署**: GitHub Pages 自动构建和发布

享受 Jekyll 开发的乐趣！🎉