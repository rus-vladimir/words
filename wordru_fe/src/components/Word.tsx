import * as React from 'react';
import { Component } from 'react';
import GameResult from '../api/core';
import Letter, { Presence } from './Letter';

interface WordProps {
    Game: GameResult
}
 
interface WordState {
    
}

const merge = (originalString:string, resultString:string) => {
    // console.log(originalString, resultString);
    if (originalString && resultString ){
        return [...originalString].map((c,ci:number) =>
            resultString[ci] == "#" 
                ? <Letter key={1000+ci} char={c} presence={Presence.Missing} />
                : 
                resultString[ci] == resultString[ci].toLowerCase() 
                    ? <Letter key={1000+ci} char={c} presence={Presence.WrongPossition} />
                    : <Letter key={1000+ci} char={c} presence={Presence.CorrectPosition} />
            )
    }
}
 
class Word extends Component<WordProps, WordState> {
    state = { Chars: String  }
    render() { 
        let arr = Array.from({length: this.props.Game.gcomplexity}, (v, i) => i);
        var rounds = this.props.Game.grounds;
        return ( 
            <div>
                {
                rounds.length > 0 
                    ?
                    <div>
                        { rounds.map((e:[string],ri:number) => 
                            <div key={7000+ri}>
                                {merge(e.at(0)!, e.at(1)!)}
                                <br/>
                            </div>)
                        }
                       {arr.map((e,i) =><Letter key={2000+i} presence={Presence.Empty} char={"_"}/>)}
                    </div >
                    :
                    <div>
                        {arr.map((e,i) =><Letter key={2000+i} presence={Presence.Empty} char={"_"}/>)}
                    </div >
                }
            </div>
        );
    }
}
 
export default Word;