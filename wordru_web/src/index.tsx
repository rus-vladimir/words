import React from "react";
import { createRoot } from "react-dom/client";
import { App } from "./App";
import { supportedLngs } from "./i18n";

createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <App SupportedLanguages={supportedLngs} />
  </React.StrictMode>,
);
