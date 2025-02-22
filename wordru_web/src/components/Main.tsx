import { useState } from "react";
import { useNewGame } from "../api/wordru.api";
import Game from "./Game";
import { v4 as uuidv4 } from "uuid";
import Logo from "../assets/logo.svg?react";
import { Lang } from "./Lang";
import i18n from "i18next";
import { Loader2Icon } from "lucide-react";
import { DarkSwitch } from "./Dark";

export function Main() {
  function newSession(): string {
    localStorage.clear();
    const session = uuidv4();
    localStorage.setItem("session", session);
    return session;
  }

  const maybeSession = localStorage.getItem("session");

  const [language, setLanguage] = useState<string>(i18n.languages[0]);

  const sessionId = maybeSession == null ? newSession() : maybeSession;

  const { data, isLoading, error, onCheckWord } = useNewGame({
    language,
    sessionId,
  });

  const onLanguageChange = (language: string) => {
    setLanguage(language);
  };

  return (
    <div className="flex-center relative m-1 h-screen flex-col dark:bg-gray-900">
      <header className="flex-center size-24 md:size-[10rem]">
        <Logo className="animate-logo-spin" />
      </header>
      <Lang OnChange={onLanguageChange} />
      <DarkSwitch />
      {(!isLoading || data != null) && (
        <Game
          Game={data!}
          HasError={error}
          OnSubmit={onCheckWord}
          Language={language}
        />
      )}

      {isLoading && (
        <Loader2Icon className="absolute size-40 animate-spin text-orange-700 opacity-30" />
      )}
    </div>
  );
}
