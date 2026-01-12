---
layout: default
title: CRAQ 技术文档
permalink: /craq/
---

<div class="page-header">
  <h1 class="page-title">CRAQ 技术文档</h1>
  <p class="page-description">Chain Replication with Apportioned Queries (CRAQ) 协议的完整技术文档和实现分析</p>
</div>

<div class="craq-intro-section">
  <div class="intro-card">
    <h2>关于 CRAQ</h2>
    <p>CRAQ (Chain Replication with Apportioned Queries) 是一种分布式存储协议，它在保持强一致性的同时提供了高可用性和良好的读性能。本文档集合包含了对 CRAQ 协议 Erlang 实现的深入分析。</p>
  </div>
</div>

<div class="craq-categories">
  <!-- 核心概念 -->
  <section class="doc-category">
    <h2 class="category-title">
      <i class="material-icons">architecture</i>
      核心概念与架构
    </h2>
    <p class="category-desc">CRAQ 协议的基础理论和架构设计</p>
    <div class="doc-grid">
      <a href="/craq/docs/CRAQ_SOURCE_FILES_ANALYSIS.html" class="doc-card">
        <h3>CRAQ 源代码文件完整解读</h3>
        <p>深入分析 CRAQ 协议的 Erlang 实现源码结构和核心模块</p>
        <span class="doc-tag">源码分析</span>
      </a>
      
      <a href="/craq/docs/Chain_Replication_Analysis.html" class="doc-card">
        <h3>Chain Replication 架构分析</h3>
        <p>链式复制协议的基础概念和架构设计原理</p>
        <span class="doc-tag">架构设计</span>
      </a>
      
      <a href="/craq/docs/Chain_Replication_Introduction_Analysis.html" class="doc-card">
        <h3>Chain Replication 入门介绍</h3>
        <p>链式复制协议的入门级介绍和基本概念</p>
        <span class="doc-tag">入门指南</span>
      </a>
      
      <a href="/craq/docs/Consensus_Protocols_Comparison.html" class="doc-card">
        <h3>共识协议对比分析</h3>
        <p>CRAQ 与其他分布式共识协议的详细对比分析</p>
        <span class="doc-tag">对比分析</span>
      </a>
    </div>
  </section>

  <!-- 协议格式 -->
  <section class="doc-category">
    <h2 class="category-title">
      <i class="material-icons">code</i>
      协议格式与序列化
    </h2>
    <p class="category-desc">二进制协议格式和数据序列化机制</p>
    <div class="doc-grid">
      <a href="/craq/docs/ERLANG_CRAQ_BINARY_FORMAT_ANALYSIS.html" class="doc-card">
        <h3>CRAQ 二进制格式分析</h3>
        <p>CRAQ 协议的二进制数据格式和编码规范详解</p>
        <span class="doc-tag">协议格式</span>
      </a>
      
      <a href="/craq/docs/ERLANG_BINARY_SERIALIZATION_MAPPING.html" class="doc-card">
        <h3>Erlang 二进制序列化映射</h3>
        <p>Erlang 环境下的二进制数据序列化和反序列化机制</p>
        <span class="doc-tag">序列化</span>
      </a>
      
      <a href="/craq/docs/TRACE_LOG_BINARY_FORMAT_DETAILED_ANALYSIS.html" class="doc-card">
        <h3>追踪日志二进制格式分析</h3>
        <p>系统追踪日志的二进制格式和解析方法</p>
        <span class="doc-tag">日志格式</span>
      </a>
      
      <a href="/craq/docs/FILE_DESCRIPTOR_STRUCTURE_ANALYSIS.html" class="doc-card">
        <h3>文件描述符结构分析</h3>
        <p>CRAQ 系统中文件描述符的内部结构和管理机制</p>
        <span class="doc-tag">数据结构</span>
      </a>
    </div>
  </section>

  <!-- 集群管理 -->
  <section class="doc-category">
    <h2 class="category-title">
      <i class="material-icons">cluster</i>
      集群管理
    </h2>
    <p class="category-desc">集群构建、节点管理和故障检测</p>
    <div class="doc-grid">
      <a href="/craq/docs/CRAQ_CLUSTER_SETUP_DETAILED_ANALYSIS.html" class="doc-card">
        <h3>CRAQ 集群构建详细分析</h3>
        <p>基于日志追踪的 CRAQ 集群构建过程完整分析</p>
        <span class="doc-tag">集群构建</span>
      </a>
      
      <a href="/craq/docs/ADD_NODE_CALL_CHAIN_ANALYSIS.html" class="doc-card">
        <h3>添加节点调用链分析</h3>
        <p>向 CRAQ 集群添加新节点的完整调用链和处理流程</p>
        <span class="doc-tag">节点管理</span>
      </a>
      
      <a href="/craq/docs/CRAQ_FAILURE_DETECTOR_ANALYSIS.html" class="doc-card">
        <h3>CRAQ 故障检测器分析</h3>
        <p>CRAQ 集群中故障检测机制的实现和优化策略</p>
        <span class="doc-tag">故障检测</span>
      </a>
      
      <a href="/craq/docs/EH_APP_CONFIG_SOURCE_ANALYSIS.html" class="doc-card">
        <h3>EH 应用配置源码分析</h3>
        <p>CRAQ 集群配置管理和应用程序配置分析</p>
        <span class="doc-tag">配置管理</span>
      </a>
    </div>
  </section>

  <!-- 快照与同步 -->
  <section class="doc-category">
    <h2 class="category-title">
      <i class="material-icons">sync</i>
      快照与数据同步
    </h2>
    <p class="category-desc">快照机制和数据同步流程</p>
    <div class="doc-grid">
      <a href="/craq/docs/SNAPSHOT_CONCEPT_COMPREHENSIVE_GUIDE.html" class="doc-card">
        <h3>快照概念综合指南</h3>
        <p>CRAQ 快照机制的完整概念介绍和实现指南</p>
        <span class="doc-tag">快照机制</span>
      </a>
      
      <a href="/craq/docs/SNAPSHOT_DATA_SYNC_FLOW_ANALYSIS.html" class="doc-card">
        <h3>快照数据同步流程分析</h3>
        <p>快照数据在集群节点间的同步流程和优化方法</p>
        <span class="doc-tag">数据同步</span>
      </a>
      
      <a href="/craq/docs/CRAQ_SNAPSHOT_MECHANISM_ANALYSIS.html" class="doc-card">
        <h3>CRAQ 快照机制分析</h3>
        <p>CRAQ 协议中快照机制的核心实现和优化策略</p>
        <span class="doc-tag">机制分析</span>
      </a>
      
      <a href="/craq/docs/CRAQ_INITIAL_CLUSTER_DATA_SYNC_ANALYSIS.html" class="doc-card">
        <h3>初始集群数据同步分析</h3>
        <p>新建集群时的初始数据同步过程和实现细节</p>
        <span class="doc-tag">初始同步</span>
      </a>
    </div>
  </section>

  <!-- 数据更新 -->
  <section class="doc-category">
    <h2 class="category-title">
      <i class="material-icons">update</i>
      数据更新机制
    </h2>
    <p class="category-desc">数据写入、更新和冲突解决</p>
    <div class="doc-grid">
      <a href="/craq/docs/ERLANG_CRAQ_COMPLETE_UPDATE_FLOW_ANALYSIS.html" class="doc-card">
        <h3>CRAQ 完整更新流程分析</h3>
        <p>CRAQ 协议中数据更新的完整流程和实现细节</p>
        <span class="doc-tag">更新流程</span>
      </a>
      
      <a href="/craq/docs/ERLANG_CRAQ_UPDATE_CALL_CHAIN.html" class="doc-card">
        <h3>CRAQ 更新调用链</h3>
        <p>数据更新操作的调用链分析和性能优化</p>
        <span class="doc-tag">调用链</span>
      </a>
      
      <a href="/craq/docs/CRAQ_WRITE_CONFLICT_RESOLVER_ANALYSIS.html" class="doc-card">
        <h3>写冲突解决器分析</h3>
        <p>CRAQ 系统中写冲突的检测和解决机制</p>
        <span class="doc-tag">冲突解决</span>
      </a>
      
      <a href="/craq/docs/ERLANG_CRAQ_INCREMENTAL_UPDATE_CODE_ANALYSIS.html" class="doc-card">
        <h3>增量更新代码分析</h3>
        <p>增量数据更新的实现方法和性能优化策略</p>
        <span class="doc-tag">增量更新</span>
      </a>
    </div>
  </section>

  <!-- 文档与指南 -->
  <section class="doc-category">
    <h2 class="category-title">
      <i class="material-icons">book</i>
      文档与使用指南
    </h2>
    <p class="category-desc">技术文档翻译和使用指南</p>
    <div class="doc-grid">
      <a href="/craq/docs/ERLANG_CRAQ_PDF_TRANSLATION.html" class="doc-card">
        <h3>Erlang CRAQ PDF 文档翻译</h3>
        <p>CRAQ 原始技术论文的完整中文翻译和解释</p>
        <span class="doc-tag">文档翻译</span>
      </a>
      
      <a href="/craq/docs/CRAQ-erlang-implementation.html" class="doc-card">
        <h3>CRAQ Erlang 实现文档</h3>
        <p>CRAQ 协议 Erlang 实现的技术文档和使用指南</p>
        <span class="doc-tag">实现文档</span>
      </a>
      
      <a href="/craq/docs/ERLANG_CRAQ_USAGE_GUIDE.html" class="doc-card">
        <h3>CRAQ 使用指南</h3>
        <p>CRAQ 系统的安装、配置和使用的完整指南</p>
        <span class="doc-tag">使用指南</span>
      </a>
    </div>
  </section>
</div>

<style>
.page-header {
  text-align: center;
  margin-bottom: 3rem;
  padding: 2rem 0;
  border-bottom: 1px solid #e5e7eb;
}

.page-title {
  font-size: 2.5rem;
  font-weight: 700;
  color: #1f2937;
  margin-bottom: 1rem;
}

.page-description {
  font-size: 1.1rem;
  color: #6b7280;
  max-width: 600px;
  margin: 0 auto;
}

.craq-intro-section {
  margin-bottom: 3rem;
}

.intro-card {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  padding: 2rem;
  border-radius: 12px;
  box-shadow: 0 10px 25px rgba(102, 126, 234, 0.2);
}

.intro-card h2 {
  font-size: 1.5rem;
  margin-bottom: 1rem;
}

.intro-card p {
  font-size: 1rem;
  line-height: 1.6;
  opacity: 0.95;
}

.doc-category {
  margin-bottom: 3rem;
}

.category-title {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 1.5rem;
  font-weight: 600;
  color: #1f2937;
  margin-bottom: 0.5rem;
}

.category-title i {
  color: #667eea;
}

.category-desc {
  color: #6b7280;
  margin-bottom: 1.5rem;
}

.doc-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
}

.doc-card {
  display: block;
  background: white;
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  padding: 1.5rem;
  text-decoration: none;
  color: inherit;
  transition: all 0.3s ease;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
  cursor: pointer;
}

.doc-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.1);
  border-color: #667eea;
}

.doc-card h3 {
  font-size: 1.1rem;
  font-weight: 600;
  color: #1f2937;
  margin-bottom: 0.5rem;
  line-height: 1.4;
}

.doc-card p {
  color: #6b7280;
  font-size: 0.9rem;
  line-height: 1.5;
  margin-bottom: 1rem;
}

.doc-tag {
  display: inline-block;
  background: #f3f4f6;
  color: #374151;
  padding: 0.25rem 0.75rem;
  border-radius: 20px;
  font-size: 0.75rem;
  font-weight: 500;
}

.doc-card:hover .doc-tag {
  background: #667eea;
  color: white;
}

.link-status {
  margin-top: 0.5rem;
  font-size: 0.8rem;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  display: none;
}

.link-status.success {
  background: #d1fae5;
  color: #065f46;
  display: block;
}

.link-status.error {
  background: #fee2e2;
  color: #991b1b;
  display: block;
}

@media (max-width: 768px) {
  .page-title {
    font-size: 2rem;
  }
  
  .doc-grid {
    grid-template-columns: 1fr;
  }
  
  .intro-card {
    padding: 1.5rem;
  }
}
</style>

<script>
document.addEventListener('DOMContentLoaded', function() {
  // 为所有文档卡片添加点击事件
  const docCards = document.querySelectorAll('.doc-card');
  
  docCards.forEach(card => {
    card.addEventListener('click', function(e) {
      e.preventDefault();
      const href = this.getAttribute('href');
      
      // 尝试多种路径
      const paths = [
        href,
        href.replace('/craq/', './craq/'),
        href.replace('/craq/', 'craq/'),
        '.' + href
      ];
      
      // 测试哪个路径可用
      testPaths(paths, this);
    });
  });
  
  function testPaths(paths, cardElement) {
    let pathIndex = 0;
    
    function tryNextPath() {
      if (pathIndex >= paths.length) {
        showStatus(cardElement, 'error', '所有路径都无法访问');
        return;
      }
      
      const currentPath = paths[pathIndex];
      
      fetch(currentPath)
        .then(response => {
          if (response.ok) {
            showStatus(cardElement, 'success', `找到文档: ${currentPath}`);
            // 在新窗口打开文档
            window.open(currentPath, '_blank');
          } else {
            pathIndex++;
            tryNextPath();
          }
        })
        .catch(error => {
          pathIndex++;
          tryNextPath();
        });
    }
    
    tryNextPath();
  }
  
  function showStatus(cardElement, type, message) {
    let statusElement = cardElement.querySelector('.link-status');
    if (!statusElement) {
      statusElement = document.createElement('div');
      statusElement.className = 'link-status';
      cardElement.appendChild(statusElement);
    }
    
    statusElement.className = `link-status ${type}`;
    statusElement.textContent = message;
    
    // 3秒后隐藏状态
    setTimeout(() => {
      statusElement.style.display = 'none';
    }, 3000);
  }
});
</script>