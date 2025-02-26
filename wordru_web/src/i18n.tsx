import i18n from "i18next";
import { initReactI18next } from "react-i18next";
import HttpBackend, { HttpBackendOptions } from "i18next-http-backend";
import LanguageDetector from "i18next-browser-languagedetector";

i18n
  .use(HttpBackend)
  .use(initReactI18next)
  .use(LanguageDetector)
  .init<HttpBackendOptions>({
    fallbackLng: "en",
    load: "languageOnly",
    keySeparator: false,
    debug: true,
    interpolation: {
      escapeValue: false,
    },
    backend: {
      loadPath: "/locales/{{lng}}/{{ns}}.json",
    },
  });
