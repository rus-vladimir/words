import { AxiosPromise } from 'axios'
import { ResponseValues, UseAxiosResult } from 'axios-hooks'
import { wordruApi } from './config/apiHooks'
import GameResult from './core'


export const useGameSession  = (sessionId:string) : [
  UseAxiosResult<GameResult, any, any>,
  (input? : string) => AxiosPromise
] => {
  const result = wordruApi({method: 'GET'}, { manual: true })

  const [, refetch] = result

  const getCheckGame = (input? : string): AxiosPromise<GameResult|null> => {
    const url = input == null 
      ? `startGame/${sessionId}` 
      : `check/${sessionId}/${input}`
    return refetch({url})
  }


  return [result, getCheckGame]
}

