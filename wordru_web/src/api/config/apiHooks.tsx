import axios from "axios";

export const wordsAxios = axios.create({
  //baseURL: "http://petrol.trade:8081/",
  baseURL: "http://localhost:8081/",
});
