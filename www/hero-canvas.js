(function () {
  "use strict";

  function initCanvas(canvas) {
    if (canvas._heroInit) return;
    canvas._heroInit = true;

    var ctx = canvas.getContext("2d");
    var particles = [];
    var NUM = 60;
    var raf;

    function resize() {
      canvas.width  = canvas.offsetWidth;
      canvas.height = canvas.offsetHeight;
    }

    function makeParticle(fromLeft) {
      var speed  = 0.2 + Math.random() * 1.4;
      var amber  = Math.random() < 0.15;
      var trail  = speed * 22;
      return {
        x:     fromLeft ? -trail : Math.random() * canvas.width,
        y:     12 + Math.random() * (canvas.height - 24),
        speed: speed,
        trail: trail,
        r:     amber ? 1.4 : 0.85,
        base:  amber ? "243,178,75" : "215,205,190",
        alpha: 0.18 + Math.random() * 0.18
      };
    }

    resize();

    var ro = new ResizeObserver(function () { resize(); });
    ro.observe(canvas);

    for (var i = 0; i < NUM; i++) {
      particles.push(makeParticle(false));
    }

    function draw() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      particles.forEach(function (p, idx) {
        var x0  = p.x - p.trail;
        var grad = ctx.createLinearGradient(x0, p.y, p.x, p.y);
        grad.addColorStop(0, "rgba(" + p.base + ",0)");
        grad.addColorStop(1, "rgba(" + p.base + "," + p.alpha + ")");

        ctx.beginPath();
        ctx.moveTo(x0, p.y);
        ctx.lineTo(p.x,  p.y);
        ctx.strokeStyle = grad;
        ctx.lineWidth   = p.r;
        ctx.stroke();

        ctx.beginPath();
        ctx.arc(p.x, p.y, p.r * 1.5, 0, Math.PI * 2);
        ctx.fillStyle = "rgba(" + p.base + "," + p.alpha + ")";
        ctx.fill();

        p.x += p.speed;

        if (p.x - p.trail > canvas.width) {
          particles[idx] = makeParticle(true);
        }
      });

      raf = requestAnimationFrame(draw);
    }

    draw();

    // Pause when tab/page hidden to save CPU
    document.addEventListener("visibilitychange", function () {
      if (document.hidden) {
        cancelAnimationFrame(raf);
      } else {
        draw();
      }
    });
  }

  function initAll() {
    document.querySelectorAll(".conduit-hero-canvas").forEach(initCanvas);
  }

  // Initial load
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initAll);
  } else {
    initAll();
  }

  // Catch lazy-rendered Shiny tabs
  $(document).on("shown.bs.tab", function () {
    setTimeout(initAll, 50);
  });
})();
