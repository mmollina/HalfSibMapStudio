// HalfSibMap Studio: small client-side helpers (no external assets).
(function () {
  function setBusy(busy) {
    document.querySelectorAll('.hsms-job-btn').forEach(function (b) {
      if (busy) {
        b.setAttribute('disabled', 'disabled');
        b.setAttribute('title', 'A job is running. Wait for it to finish or cancel it in the Jobs panel.');
      } else {
        b.removeAttribute('disabled');
        b.removeAttribute('title');
      }
    });
    document.body.classList.toggle('hsms-busy', !!busy);
  }
  var busyState = false;
  if (window.Shiny) {
    Shiny.addCustomMessageHandler('hsms-busy', function (msg) { busyState = !!msg.busy; setBusy(busyState); });
  } else {
    document.addEventListener('DOMContentLoaded', function () {
      Shiny.addCustomMessageHandler('hsms-busy', function (msg) { busyState = !!msg.busy; setBusy(busyState); });
    });
  }
  // Buttons rendered after a job started must also be disabled.
  document.addEventListener('shiny:value', function () { if (busyState) setTimeout(function () { setBusy(true); }, 0); });
})();
