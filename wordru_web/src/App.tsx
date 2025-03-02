import "./index.css";
import { library } from "@fortawesome/fontawesome-svg-core";
import { faBroom, faDeleteLeft } from "@fortawesome/free-solid-svg-icons";
library.add(faBroom, faDeleteLeft);

import { Main } from "./components/Main";
import { useTranslation } from "react-i18next";
import { useEffect, useState } from "react";

interface AppProps {
  SupportedLanguages: any;
}

export function App(props: AppProps) {
  const { i18n } = useTranslation();
  useEffect(() => {
    if (i18n.resolvedLanguage) {
      setLanguage(i18n.resolvedLanguage);
    }
  }, [i18n.resolvedLanguage]);

  const [language, setLanguage] = useState<string>(i18n.resolvedLanguage!);

  const onLanguageChange = (language: string) => {
    setLanguage(language);
  };

  return (
    <Main
      Language={language}
      SupportedLanguages={Object.entries(props.SupportedLanguages)}
      OnChange={onLanguageChange}
    />
  );
}
