
import axios from 'axios'
import { makeUseAxios } from 'axios-hooks'

const wordruAxios = axios.create({ 
        baseURL: "http://localhost:8081/"
    })

export const wordruApi = makeUseAxios({ axios: wordruAxios })