import Arizona from '@arizona-framework/client';
globalThis.arizona = new Arizona({ logLevel: 'debug' });
arizona.connect({ wsPath: '/live' });

// Code Carousel functionality
let currentSlide = 0;
const slides = document.querySelectorAll('.carousel-slide');
const dots = document.querySelectorAll('.carousel-dot');
const slidesContainer = document.getElementById('carousel-slides');

function showSlide(index) {
  // Update transform to slide to the correct position
  const translateX = -index * 33.333333; // Each slide is 33.333333% wide
  if (slidesContainer) {
    slidesContainer.style.transform = `translateX(${translateX}%)`;
  }

  // Toggle scroll on slides: only active slide can scroll
  slides.forEach((slide, i) => {
    if (i === index) {
      slide.classList.remove('overflow-hidden');
      slide.classList.add('overflow-x-auto');
    } else {
      slide.classList.remove('overflow-x-auto');
      slide.classList.add('overflow-hidden');
    }
  });

  // Update dots
  dots.forEach(dot => {
    dot.classList.remove('active');
    dot.classList.remove('bg-erlang-red');
    dot.classList.add('bg-gray-600');
  });

  // Update file name and label
  if (slides[index]) {
    // Update file name
    const fileName = slides[index].getAttribute('data-file');
    const fileNameEl = document.getElementById('file-name');
    if (fileNameEl && fileName) {
      fileNameEl.textContent = fileName;
    }

    // Update label
    const label = slides[index].getAttribute('data-label');
    const labelEl = document.getElementById('example-label');
    if (labelEl && label) {
      labelEl.textContent = label;
    }

    // Update dot
    if (dots[index]) {
      dots[index].classList.add('active');
      dots[index].classList.add('bg-erlang-red');
      dots[index].classList.remove('bg-gray-600');
    }
  }
}

function nextExample() {
  currentSlide = (currentSlide + 1) % slides.length;
  showSlide(currentSlide);
}

function prevExample() {
  currentSlide = (currentSlide - 1 + slides.length) % slides.length;
  showSlide(currentSlide);
}

function goToExample(index) {
  currentSlide = index;
  showSlide(currentSlide);
}

// Make functions global so they can be called from HTML onclick
globalThis.nextExample = nextExample;
globalThis.prevExample = prevExample;
globalThis.goToExample = goToExample;

// Initialize carousel when DOM is loaded
document.addEventListener('DOMContentLoaded', function() {
  showSlide(0);
});
