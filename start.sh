#!/bin/bash

echo "🚀 Hexo 博客本地开发服务器"
echo "=========================="

# 检查端口
check_port() {
    if lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo "⚠️  端口 $1 被占用，尝试清理..."
        lsof -ti:$1 | xargs kill -9 2>/dev/null || true
        sleep 1
    fi
}

# 检查 Hexo 是否安装
if ! command -v hexo >/dev/null 2>&1; then
    echo "❌ Hexo 未安装"
    echo "请运行: npm install -g hexo-cli"
    exit 1
fi

echo "✅ Hexo 已安装"

# 检查依赖
if [ ! -d "node_modules" ]; then
    echo "📦 安装依赖..."
    npm install
fi

# 清理并生成
echo "🧹 清理旧文件..."
hexo clean

# 检查端口
check_port 4000

echo ""
echo "🌟 启动 Hexo 服务器..."
echo "📍 访问地址: http://localhost:4000"
echo "🔄 按 Ctrl+C 停止服务器"
echo ""

# 启动服务器
hexo server