import { wordsAxios } from "./config/apiHooks";
import GameResult from "./core";
import { useEffect, useState } from "react";

interface GameSessionProps {
  sessionId: string;
  language: string;
}

export function useNewGame(props: GameSessionProps) {
  const [data, setData] = useState<GameResult>();
  const [error, setError] = useState(false);
  const [isLoading, setIsLoading] = useState(true);

  function onCheckWord(input?: string) {
    // Handle initial delay before showing spiner
    let hasLoaded = false;
    setIsLoading(false);
    setTimeout(() => {
      if (!hasLoaded) {
        setIsLoading(true);
      }
    }, 1000);

    wordsAxios
      .get<GameResult>(`check/${props.language}/${props.sessionId}/${input}`)
      .then((res) => {
        if (res.status === 200) {
          setData(res.data);
        }
      })
      .catch((error) => {
        if (error.status == 400) {
          setError(error);
          // Show error only for 3 sec
          setTimeout(() => {
            setError(false);
          }, 3000);
        }
      })
      .finally(() => {
        hasLoaded = true;
        setIsLoading(false);
      });
  }

  var gameStarted = () =>
    wordsAxios
      .get<GameResult>(`startGame/${props.language}/${props.sessionId}`)
      .then((res) => {
        if (res.status === 200) {
          setData(res.data);
        }
      })
      .catch((error) => {
        setError(error);
      })
      .finally(() => {
        setIsLoading(false);
      });

  useEffect(() => {
    gameStarted();
  }, [props.language, props.sessionId]);

  return { data, isLoading, error, onCheckWord };
}
