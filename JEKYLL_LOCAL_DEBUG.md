# 🔧 Jekyll 本地样式问题诊断与解决方案

## 🎯 问题描述

您遇到的问题是典型的 Jekyll 本地开发环境样式加载问题：
- ✅ GitHub Pages 上显示正常（有完整样式）
- ❌ 本地 Jekyll 服务器显示异常（缺少样式，只有基本 HTML）

## 🔍 问题根本原因

### 1. **路径解析问题**
```yaml
# GitHub Pages 环境
baseurl: ""
url: "https://hzsunzixiang.github.io"

# 本地开发环境
baseurl: ""  # 应该为空
url: "http://localhost:4000"
```

### 2. **CSS/JS 文件路径问题**
```html
<!-- 问题路径（可能失效） -->
<link rel="stylesheet" href="{{ "/assets/css/indigo.css" | relative_url }}">

<!-- 修复后的路径 -->
<link rel="stylesheet" href="{{ '/assets/css/indigo.css' | prepend: site.baseurl }}">
```

### 3. **Jekyll 配置差异**
- GitHub Pages 使用特定的 Jekyll 版本和插件
- 本地环境可能版本不匹配或配置不同

## ✅ 解决方案

### 方案一：使用修复后的 Jekyll 服务器（推荐）

#### 1. 安装 Jekyll 和依赖
```bash
# 安装 Ruby 和 Bundler（如果没有）
gem install bundler jekyll

# 安装项目依赖
bundle install
```

#### 2. 使用修复后的启动脚本
```bash
# 使用修复版启动脚本
./start-jekyll-fixed.sh

# 或者手动启动
bundle exec jekyll serve --config _config.yml,_config_local.yml --host 0.0.0.0 --port 4000 --livereload
```

#### 3. 访问地址
- **主页**: http://localhost:4000
- **LiveReload**: 自动刷新功能已启用

### 方案二：使用简化的预览服务器

如果 Jekyll 安装有问题，可以使用简化版本：

```bash
# 启动简化服务器
./start-simple.sh

# 访问 http://localhost:4000（或其他可用端口）
```

### 方案三：Docker 方式（推荐给不想安装 Ruby 的用户）

```bash
# 使用 Docker Compose
docker-compose up

# 或者直接使用 Docker
docker run --rm -it \
  -p 4000:4000 \
  -p 35729:35729 \
  -v "$(pwd):/srv/jekyll" \
  jekyll/jekyll:4.2.2 \
  jekyll serve --host 0.0.0.0 --livereload
```

## 🛠️ 已实施的修复

### 1. **配置文件修复**
- ✅ 创建了 `_config_local.yml` 本地开发配置
- ✅ 修复了 `baseurl` 和 `url` 设置
- ✅ 启用了增量构建和实时重载

### 2. **布局文件修复**
- ✅ 修复了 CSS 文件路径：`{{ '/assets/css/indigo.css' | prepend: site.baseurl }}`
- ✅ 修复了 JS 文件路径：`{{ '/assets/js/indigo.js' | prepend: site.baseurl }}`
- ✅ 备份了原始文件：`_layouts/default.html.backup`

### 3. **启动脚本优化**
- ✅ 创建了 `start-jekyll-fixed.sh`（完整 Jekyll 功能）
- ✅ 创建了 `start-simple.sh`（简化预览）
- ✅ 自动端口检测和清理
- ✅ 增量构建和实时重载

## 🔍 问题诊断步骤

### 1. **检查 CSS 文件是否可访问**
```bash
# 启动服务器后，直接访问 CSS 文件
curl http://localhost:4000/assets/css/indigo.css

# 或在浏览器中访问
http://localhost:4000/assets/css/indigo.css
```

### 2. **检查浏览器控制台**
1. 打开浏览器开发者工具（F12）
2. 查看 Console 标签页的错误信息
3. 查看 Network 标签页，看哪些资源加载失败

### 3. **检查 Jekyll 构建日志**
```bash
# 查看详细构建信息
bundle exec jekyll serve --verbose

# 检查构建错误
bundle exec jekyll build --verbose
```

## 🎯 常见问题和解决方案

### 问题1：`bundle: command not found`
```bash
# 安装 Bundler
gem install bundler

# 如果权限问题
sudo gem install bundler
```

### 问题2：`jekyll: command not found`
```bash
# 安装 Jekyll
gem install jekyll

# 或通过 Bundler 安装
bundle install
```

### 问题3：端口被占用
```bash
# 查看端口占用
lsof -i :4000

# 杀死占用进程
lsof -ti:4000 | xargs kill -9

# 使用其他端口
bundle exec jekyll serve --port 4001
```

### 问题4：权限问题
```bash
# 使用用户级 gem 安装
gem install --user-install jekyll bundler

# 或者设置 gem 路径
export GEM_HOME="$HOME/.gem"
export PATH="$HOME/.gem/bin:$PATH"
```

### 问题5：Ruby 版本不兼容
```bash
# 使用 rbenv 管理 Ruby 版本
brew install rbenv
rbenv install 3.0.0
rbenv global 3.0.0

# 重新安装 gems
gem install bundler jekyll
bundle install
```

## 📊 性能优化建议

### 1. **启用增量构建**
```bash
bundle exec jekyll serve --incremental
```

### 2. **排除不必要的文件**
```yaml
# _config_local.yml
exclude:
  - node_modules
  - .git
  - "*.py"
  - "*.sh"
  - README.md
```

### 3. **使用本地配置覆盖**
```bash
# 使用多个配置文件
bundle exec jekyll serve --config _config.yml,_config_local.yml
```

## 🎉 验证修复效果

修复成功后，您应该看到：

1. **完整的页面样式**：
   - ✅ 侧边栏导航
   - ✅ 现代化的设计
   - ✅ 响应式布局
   - ✅ 图标和字体

2. **功能正常**：
   - ✅ 导航链接工作
   - ✅ 文章页面正常显示
   - ✅ 实时重载功能
   - ✅ 移动端适配

3. **性能良好**：
   - ✅ 快速加载
   - ✅ 增量构建
   - ✅ 自动刷新

## 📚 相关资源

- [Jekyll 官方文档](https://jekyllrb.com/docs/)
- [GitHub Pages 本地测试](https://docs.github.com/en/pages/setting-up-a-github-pages-site-with-jekyll/testing-your-github-pages-site-locally-with-jekyll)
- [Jekyll 配置参考](https://jekyllrb.com/docs/configuration/)
- [Liquid 模板语法](https://shopify.github.io/liquid/)

---

## 🆘 如果问题仍然存在

1. **检查浏览器控制台错误**
2. **尝试强制刷新**（Ctrl+F5 或 Cmd+Shift+R）
3. **清除浏览器缓存**
4. **检查防火墙设置**
5. **尝试不同的浏览器**

如果以上方法都不能解决问题，请提供：
- 浏览器控制台的错误信息
- Jekyll 服务器的启动日志
- 具体的错误截图

这样可以进行更精确的诊断和修复。