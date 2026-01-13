#!/bin/bash

echo "🚀 启动 Jekyll 博客本地服务器"
echo "================================"

# 检查端口是否被占用
check_port() {
    if lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null ; then
        return 1
    else
        return 0
    fi
}

# 查找可用端口
find_available_port() {
    local ports=(4000 4001 4002 8080 8081 8082 3000 3001 5000)
    for port in "${ports[@]}"; do
        if check_port $port; then
            echo $port
            return 0
        fi
    done
    echo "找不到可用端口"
    return 1
}

# 方法1: 尝试 Jekyll
echo "🔍 检查 Jekyll..."
if command -v jekyll >/dev/null 2>&1; then
    echo "✅ Jekyll 已安装"
    port=$(find_available_port)
    if [ "$port" != "找不到可用端口" ]; then
        echo "🌟 启动 Jekyll 服务器..."
        echo "📍 访问地址: http://localhost:$port"
        echo "🔄 按 Ctrl+C 停止服务器"
        echo ""
        bundle exec jekyll serve --host 0.0.0.0 --port $port --livereload 2>/dev/null || jekyll serve --host 0.0.0.0 --port $port
        exit 0
    fi
fi

# 方法2: 尝试 Python3
echo "🔍 检查 Python3..."
if command -v python3 >/dev/null 2>&1; then
    echo "✅ Python3 已安装"
    port=$(find_available_port)
    if [ "$port" != "找不到可用端口" ]; then
        echo "🐍 启动 Python HTTP 服务器..."
        echo "📍 访问地址: http://localhost:$port"
        echo "⚠️  注意: 这只是静态文件服务器，不会处理 Jekyll 模板"
        echo "🔄 按 Ctrl+C 停止服务器"
        echo ""
        python3 -m http.server $port
        exit 0
    fi
fi

# 方法3: 尝试 Python2
echo "🔍 检查 Python2..."
if command -v python >/dev/null 2>&1; then
    echo "✅ Python2 已安装"
    port=$(find_available_port)
    if [ "$port" != "找不到可用端口" ]; then
        echo "🐍 启动 Python HTTP 服务器..."
        echo "📍 访问地址: http://localhost:$port"
        echo "⚠️  注意: 这只是静态文件服务器，不会处理 Jekyll 模板"
        echo "🔄 按 Ctrl+C 停止服务器"
        echo ""
        python -m SimpleHTTPServer $port
        exit 0
    fi
fi

# 方法4: 尝试 Node.js
echo "🔍 检查 Node.js..."
if command -v node >/dev/null 2>&1; then
    echo "✅ Node.js 已安装"
    port=$(find_available_port)
    if [ "$port" != "找不到可用端口" ]; then
        echo "📦 启动 Node.js HTTP 服务器..."
        echo "📍 访问地址: http://localhost:$port"
        echo "⚠️  注意: 这只是静态文件服务器，不会处理 Jekyll 模板"
        echo "🔄 按 Ctrl+C 停止服务器"
        echo ""
        npx http-server -p $port -o
        exit 0
    fi
fi

# 如果都没有找到
echo "❌ 没有找到可用的服务器"
echo ""
echo "💡 建议安装以下工具之一:"
echo "   1. Jekyll: gem install jekyll bundler"
echo "   2. Python: brew install python"
echo "   3. Node.js: brew install node"
echo ""
echo "📚 详细说明请查看 LOCAL_DEVELOPMENT.md 文件"