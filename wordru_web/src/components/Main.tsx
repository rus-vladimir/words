import { useNewGame } from "../api/wordru.api";
import Game from "./Game";
import { v4 as uuidv4 } from "uuid";
import Logo from "../assets/logo.svg?react";
import { Lang } from "./Lang";
import { Loader2Icon } from "lucide-react";
import { DarkSwitch } from "./Dark";

interface MainProps {
  SupportedLanguages: [lang: string, name: string][];
  Language: string;
  OnChange: (language: string) => void;
}

export function Main(props: MainProps) {
  function newSession(): string {
    localStorage.clear();
    const session = uuidv4();
    localStorage.setItem("session", session);
    return session;
  }

  const maybeSession = localStorage.getItem("session");

  const sessionId = maybeSession == null ? newSession() : maybeSession;

  const { data, isLoading, error, onCheckWord } = useNewGame({
    Language: props.Language,
    SessionId: sessionId,
  });

  return (
    <div className="flex-center relative m-1 h-screen flex-col">
      <div className="flex-center size-24 md:size-[10rem]">
        <Logo className="animate-logo-spin size-24" />
      </div>
      <div>
        <Lang
          OnChange={props.OnChange}
          SupportedLanguages={props.SupportedLanguages}
          Language={props.Language}
        />
        <DarkSwitch />
        {(!isLoading || data != null) && (
          <Game
            Game={data!}
            HasError={error}
            OnSubmit={onCheckWord}
            Language={props.Language}
          />
        )}

        {isLoading && (
          <Loader2Icon className="absolute size-40 animate-spin text-orange-700 opacity-30" />
        )}
      </div>
    </div>
  );
}
