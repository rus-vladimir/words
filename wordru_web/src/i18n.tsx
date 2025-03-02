import { initReactI18next } from "react-i18next";
import HttpBackend, { HttpBackendOptions } from "i18next-http-backend";
import LanguageDetector from "i18next-browser-languagedetector";
import i18n from "i18next";

export const supportedLngs = {
  en: "English",
  ru: "Русский",
  nl: "Nederlands",
  ro: "Română",
};

i18n
  .use(HttpBackend)
  .use(initReactI18next)
  .use(LanguageDetector)
  .init<HttpBackendOptions>({
    fallbackLng: "en",
    load: "languageOnly",
    supportedLngs: Object.keys(supportedLngs),
    keySeparator: false,
    debug: false,
    interpolation: {
      escapeValue: false,
    },
    backend: {
      loadPath: "/locales/{{lng}}/{{ns}}.json",
    },
  });
