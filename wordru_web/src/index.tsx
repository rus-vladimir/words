import React from "react";
import { createRoot } from "react-dom/client";
import "./index.css";
import { App } from "./App";
import { library } from "@fortawesome/fontawesome-svg-core";
import { faBroom, faDeleteLeft } from "@fortawesome/free-solid-svg-icons";
import "./i18n";

library.add(faBroom, faDeleteLeft);

createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
);
