import { useEffect, useState } from "react";
import GameResult from "../api/core";
import { useGameSession} from "../api/wordru.api";
import Game from "./Game";
import { v4 as uuidv4 } from 'uuid';
 
const Main: React.FC = () => {
    let maybeSession = localStorage.getItem('session')
    function newSession()  : string {
        localStorage.clear();
        const session = uuidv4();
        localStorage.setItem('session', session);
        return session;
    }
    
    const session = maybeSession == null? newSession() : maybeSession;

    const [, getGameResultAfterCheck] = useGameSession(session);

    const [gameResult, setGameResult] = useState<GameResult>();
    const [initialized, setInitialized] = useState(false);

    
    useEffect(()=>{
        fetchGame();
    }, []);
    

    const fetchGame = async () => {
        const {data} = await getGameResultAfterCheck();
        setGameResult(data);
        setInitialized(true);
      }
      
    

    const onSubmit = async (x:string ) => {
        const gameResultInit2 = await getGameResultAfterCheck(x);
        setGameResult(gameResultInit2.data);
        setInitialized(true);
        
    }

    return (
        <div>
            {initialized && <Game Game={gameResult!} OnSubmit={ onSubmit}/>}
        </div>
    );
}
 
export default Main;

