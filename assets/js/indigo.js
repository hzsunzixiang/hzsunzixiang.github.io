// Indigo Theme JavaScript with Sidebar

document.addEventListener('DOMContentLoaded', function() {
  // Sidebar Toggle for Mobile
  const sidebarToggle = document.getElementById('sidebar-toggle');
  const sidebar = document.getElementById('sidebar');
  const sidebarOverlay = document.getElementById('sidebar-overlay');
  
  if (sidebarToggle && sidebar && sidebarOverlay) {
    // Toggle sidebar
    sidebarToggle.addEventListener('click', function() {
      sidebar.classList.toggle('active');
      sidebarOverlay.classList.toggle('active');
      document.body.classList.toggle('sidebar-open');
    });
    
    // Close sidebar when clicking overlay
    sidebarOverlay.addEventListener('click', function() {
      sidebar.classList.remove('active');
      sidebarOverlay.classList.remove('active');
      document.body.classList.remove('sidebar-open');
    });
    
    // Close sidebar on window resize if mobile
    window.addEventListener('resize', function() {
      if (window.innerWidth > 768) {
        sidebar.classList.remove('active');
        sidebarOverlay.classList.remove('active');
        document.body.classList.remove('sidebar-open');
      }
    });
  }

  // Back to Top Button
  const backToTopButton = document.getElementById('back-to-top');
  
  if (backToTopButton) {
    window.addEventListener('scroll', function() {
      if (window.pageYOffset > 300) {
        backToTopButton.classList.add('visible');
      } else {
        backToTopButton.classList.remove('visible');
      }
    });

    backToTopButton.addEventListener('click', function() {
      window.scrollTo({
        top: 0,
        behavior: 'smooth'
      });
    });
  }

  // Smooth Scrolling for Anchor Links
  const anchorLinks = document.querySelectorAll('a[href^="#"]');
  
  anchorLinks.forEach(function(link) {
    link.addEventListener('click', function(e) {
      e.preventDefault();
      
      const targetId = this.getAttribute('href').substring(1);
      const targetElement = document.getElementById(targetId);
      
      if (targetElement) {
        const headerOffset = 80;
        const elementPosition = targetElement.getBoundingClientRect().top;
        const offsetPosition = elementPosition + window.pageYOffset - headerOffset;

        window.scrollTo({
          top: offsetPosition,
          behavior: 'smooth'
        });
      }
    });
  });

  // Add Ripple Effect to Interactive Elements
  function createRipple(event) {
    const element = event.currentTarget;
    const circle = document.createElement('span');
    const diameter = Math.max(element.clientWidth, element.clientHeight);
    const radius = diameter / 2;

    circle.style.width = circle.style.height = `${diameter}px`;
    circle.style.left = `${event.clientX - element.offsetLeft - radius}px`;
    circle.style.top = `${event.clientY - element.offsetTop - radius}px`;
    circle.classList.add('ripple');

    const ripple = element.getElementsByClassName('ripple')[0];

    if (ripple) {
      ripple.remove();
    }

    element.appendChild(circle);
  }

  const interactiveElements = document.querySelectorAll('.btn, .nav-link, .post-card, .category-card');
  interactiveElements.forEach(function(element) {
    element.addEventListener('click', createRipple);
  });

  // Lazy Loading for Images
  if ('IntersectionObserver' in window) {
    const imageObserver = new IntersectionObserver(function(entries, observer) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          const img = entry.target;
          if (img.dataset.src) {
            img.src = img.dataset.src;
            img.classList.remove('lazy');
            imageObserver.unobserve(img);
          }
        }
      });
    });

    const lazyImages = document.querySelectorAll('img[data-src]');
    lazyImages.forEach(function(img) {
      imageObserver.observe(img);
    });
  }

  // Code Block Copy Functionality
  const codeBlocks = document.querySelectorAll('pre code');
  
  codeBlocks.forEach(function(codeBlock) {
    const pre = codeBlock.parentNode;
    const copyButton = document.createElement('button');
    copyButton.className = 'copy-code-btn';
    copyButton.innerHTML = '<i class="material-icons">content_copy</i>';
    copyButton.title = 'Copy code';
    
    pre.style.position = 'relative';
    pre.appendChild(copyButton);
    
    copyButton.addEventListener('click', function() {
      const text = codeBlock.textContent;
      
      if (navigator.clipboard) {
        navigator.clipboard.writeText(text).then(function() {
          copyButton.innerHTML = '<i class="material-icons">check</i>';
          setTimeout(function() {
            copyButton.innerHTML = '<i class="material-icons">content_copy</i>';
          }, 2000);
        });
      } else {
        // Fallback for older browsers
        const textArea = document.createElement('textarea');
        textArea.value = text;
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand('copy');
        document.body.removeChild(textArea);
        
        copyButton.innerHTML = '<i class="material-icons">check</i>';
        setTimeout(function() {
          copyButton.innerHTML = '<i class="material-icons">content_copy</i>';
        }, 2000);
      }
    });
  });

  // Active Navigation Highlighting
  function updateActiveNavigation() {
    const currentPath = window.location.pathname;
    const navLinks = document.querySelectorAll('.sidebar .nav-link');
    
    navLinks.forEach(function(link) {
      const linkPath = new URL(link.href).pathname;
      if (linkPath === currentPath || (currentPath !== '/' && linkPath !== '/' && currentPath.includes(linkPath))) {
        link.classList.add('active');
      } else {
        link.classList.remove('active');
      }
    });
  }

  // Update active navigation on page load
  updateActiveNavigation();

  // Sidebar scroll behavior
  const sidebarNav = document.querySelector('.sidebar-nav');
  if (sidebarNav) {
    let isScrolling = false;
    
    sidebarNav.addEventListener('scroll', function() {
      if (!isScrolling) {
        window.requestAnimationFrame(function() {
          // Add scroll shadow effect
          if (sidebarNav.scrollTop > 0) {
            sidebar.classList.add('scrolled');
          } else {
            sidebar.classList.remove('scrolled');
          }
          isScrolling = false;
        });
        isScrolling = true;
      }
    });
  }

  // Search functionality (if search elements exist)
  const searchToggle = document.getElementById('search-toggle');
  const searchInput = document.getElementById('search-input');
  const searchResults = document.getElementById('search-results');
  
  if (searchToggle && searchInput) {
    searchToggle.addEventListener('click', function() {
      searchInput.focus();
    });
  }

  // Theme persistence
  function initializeTheme() {
    const savedTheme = localStorage.getItem('indigo-theme') || 'light';
    document.documentElement.setAttribute('data-theme', savedTheme);
  }

  // Initialize theme on load
  initializeTheme();

  // Keyboard shortcuts
  document.addEventListener('keydown', function(e) {
    // ESC to close sidebar on mobile
    if (e.key === 'Escape' && sidebar && sidebar.classList.contains('active')) {
      sidebar.classList.remove('active');
      sidebarOverlay.classList.remove('active');
      document.body.classList.remove('sidebar-open');
    }
    
    // Ctrl/Cmd + K for search (if implemented)
    if ((e.ctrlKey || e.metaKey) && e.key === 'k' && searchInput) {
      e.preventDefault();
      searchInput.focus();
    }
  });

  // Progressive enhancement for animations
  function reduceMotion() {
    return window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  }

  if (reduceMotion()) {
    document.documentElement.classList.add('reduce-motion');
  }

  // Performance optimization: Debounce scroll events
  function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
      const later = () => {
        clearTimeout(timeout);
        func(...args);
      };
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
    };
  }

  // Optimized scroll handler
  const optimizedScrollHandler = debounce(function() {
    // Handle scroll-based animations or effects here
    const scrollTop = window.pageYOffset;
    
    // Update back to top button
    if (backToTopButton) {
      if (scrollTop > 300) {
        backToTopButton.classList.add('visible');
      } else {
        backToTopButton.classList.remove('visible');
      }
    }
  }, 16); // ~60fps

  window.addEventListener('scroll', optimizedScrollHandler);

  // Initialize tooltips (if needed)
  function initializeTooltips() {
    const tooltipElements = document.querySelectorAll('[data-tooltip]');
    tooltipElements.forEach(function(element) {
      element.addEventListener('mouseenter', function() {
        // Create and show tooltip
        const tooltip = document.createElement('div');
        tooltip.className = 'tooltip';
        tooltip.textContent = this.getAttribute('data-tooltip');
        document.body.appendChild(tooltip);
        
        // Position tooltip
        const rect = this.getBoundingClientRect();
        tooltip.style.left = rect.left + (rect.width / 2) - (tooltip.offsetWidth / 2) + 'px';
        tooltip.style.top = rect.top - tooltip.offsetHeight - 8 + 'px';
        
        this._tooltip = tooltip;
      });
      
      element.addEventListener('mouseleave', function() {
        if (this._tooltip) {
          document.body.removeChild(this._tooltip);
          this._tooltip = null;
        }
      });
    });
  }

  // Initialize tooltips
  initializeTooltips();
});

// Add CSS for new features
const additionalCSS = `
  .ripple {
    position: absolute;
    border-radius: 50%;
    background-color: rgba(255, 255, 255, 0.6);
    transform: scale(0);
    animation: ripple-animation 0.6s linear;
    pointer-events: none;
  }
  
  @keyframes ripple-animation {
    to {
      transform: scale(4);
      opacity: 0;
    }
  }
  
  .copy-code-btn {
    position: absolute;
    top: 10px;
    right: 10px;
    background: rgba(255, 255, 255, 0.1);
    border: none;
    color: white;
    padding: 8px;
    border-radius: 4px;
    cursor: pointer;
    opacity: 0.7;
    transition: opacity 0.3s;
  }
  
  .copy-code-btn:hover {
    opacity: 1;
  }
  
  .sidebar.scrolled::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 1px;
    background: var(--border-color);
    box-shadow: 0 1px 3px var(--shadow-color);
  }
  
  .tooltip {
    position: absolute;
    background: var(--text-primary);
    color: var(--text-white);
    padding: 6px 12px;
    border-radius: 4px;
    font-size: 0.875rem;
    z-index: 10000;
    pointer-events: none;
    opacity: 0;
    animation: tooltip-show 0.2s ease forwards;
  }
  
  @keyframes tooltip-show {
    to {
      opacity: 1;
    }
  }
  
  .reduce-motion * {
    animation-duration: 0.01ms !important;
    animation-iteration-count: 1 !important;
    transition-duration: 0.01ms !important;
  }
  
  body.sidebar-open {
    overflow: hidden;
  }
  
  @media (max-width: 768px) {
    body.sidebar-open .main-wrapper {
      filter: blur(2px);
    }
  }
`;

// Inject additional CSS
const style = document.createElement('style');
style.textContent = additionalCSS;
document.head.appendChild(style);