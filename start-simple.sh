#!/bin/bash

echo "🚀 启动简单的本地预览服务器"
echo "============================="

# 检查端口
check_port() {
    if lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo "⚠️  端口 $1 被占用，尝试清理..."
        lsof -ti:$1 | xargs kill -9 2>/dev/null || true
        sleep 2
    fi
}

# 查找可用端口
find_port() {
    for port in 4000 4001 4002 8080 8081 3000; do
        if ! lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
            echo $port
            return
        fi
    done
    echo "8888"  # 默认端口
}

# 创建临时的 index.html（如果不存在）
if [ ! -f "index.html" ]; then
    echo "📝 创建临时首页..."
    cat > index.html << 'EOF'
<!DOCTYPE html>
<html>
<head>
    <title>Jekyll 博客预览</title>
    <meta charset="utf-8">
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
        .container { max-width: 800px; margin: 0 auto; }
        .post { margin: 30px 0; padding: 20px; border: 1px solid #ddd; }
        .nav { margin: 20px 0; }
        .nav a { margin-right: 20px; text-decoration: none; color: #0066cc; }
    </style>
</head>
<body>
    <div class="container">
        <h1>Jekyll 博客本地预览</h1>
        <div class="nav">
            <a href="/">首页</a>
            <a href="/about.html">关于</a>
            <a href="/archive.html">归档</a>
            <a href="/craq/">CRAQ</a>
        </div>
        
        <div class="post">
            <h2>欢迎访问本地预览</h2>
            <p>这是一个简化的预览页面。要查看完整的 Jekyll 功能，请安装 Jekyll 并使用 Jekyll 服务器。</p>
            
            <h3>可用页面：</h3>
            <ul>
                <li><a href="/about.html">关于页面</a></li>
                <li><a href="/archive.html">文章归档</a></li>
                <li><a href="/craq/">CRAQ 文档</a></li>
                <li><a href="/_posts/">文章目录</a></li>
            </ul>
            
            <h3>最新文章：</h3>
            <ul>
                <li><a href="/_posts/2026-01-13-dma-architecture-analysis.md">DMA 架构分析与设计图解</a></li>
                <li><a href="/_posts/2024-01-15-welcome-to-my-blog.md">Welcome to My Blog</a></li>
            </ul>
        </div>
        
        <div class="post">
            <h3>🔧 Jekyll 问题诊断</h3>
            <p>如果您看到这个页面，说明：</p>
            <ul>
                <li>✅ 服务器运行正常</li>
                <li>⚠️ Jekyll 可能未安装或配置有问题</li>
            </ul>
            
            <h4>解决方案：</h4>
            <ol>
                <li>安装 Jekyll: <code>gem install jekyll bundler</code></li>
                <li>安装依赖: <code>bundle install</code></li>
                <li>启动 Jekyll: <code>bundle exec jekyll serve</code></li>
            </ol>
        </div>
    </div>
</body>
</html>
EOF
fi

# 清理端口
port=$(find_port)
check_port $port

echo "🌐 启动 Python HTTP 服务器..."
echo "📍 访问地址: http://localhost:$port"
echo "🔄 按 Ctrl+C 停止服务器"
echo ""
echo "💡 提示：这是静态文件服务器，不会处理 Jekyll 模板"
echo "   要获得完整功能，请使用 Jekyll 服务器"
echo ""

# 启动服务器
python3 -m http.server $port