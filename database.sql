--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.4
-- Dumped by pg_dump version 9.6.4

-- Started on 2017-08-18 18:51:56 CEST

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 1 (class 3079 OID 12399)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2140 (class 0 OID 0)
-- Dependencies: 1
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 185 (class 1259 OID 16385)
-- Name: topic; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE topic (
    id integer NOT NULL,
    content jsonb NOT NULL
);


ALTER TABLE topic OWNER TO postgres;

--
-- TOC entry 186 (class 1259 OID 16391)
-- Name: topic_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE topic_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE topic_id_seq OWNER TO postgres;

--
-- TOC entry 2141 (class 0 OID 0)
-- Dependencies: 186
-- Name: topic_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE topic_id_seq OWNED BY topic.id;


--
-- TOC entry 2012 (class 2604 OID 16393)
-- Name: topic id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY topic ALTER COLUMN id SET DEFAULT nextval('topic_id_seq'::regclass);


--
-- TOC entry 2132 (class 0 OID 16385)
-- Dependencies: 185
-- Data for Name: topic; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO topic VALUES (1, '{"body": "Вашей женщине её коллега/знакомый мужчина намеревается подарить ювелирное украшение", "refs": [], "title": "Дорогие подарки", "subtitle": "Ситуация", "questions": ["Как отреагировать вашей женщине?", "Справедливы ли ваши рассуждения в обратной ситуации когда подарок дарит женшина мужчине?"], "description": "О том, что подарки не всегда являются безобидными"}');
INSERT INTO topic VALUES (2, '{"body": "Вам нравится творчество какого-нибудь музыканта или киноартиста, но его политические взгляды или личные позиции по иным, принципиальным для вас вопросам противоречат вашим.", "refs": [], "title": "Артисты и их политические взгляды", "subtitle": "Ситуация", "questions": ["будете ли вы наслаждаться творчеством этого человека, если оно непосредственно не затрагивает принципиальные различия?"], "description": "О целостном восприятии артистов вне зависимости от их политических взглядов"}');
INSERT INTO topic VALUES (3, '{"body": "Вы узнаёте, что два человека одинакового пола (gay-пара) усыновили и воспитывают ребёнка", "refs": [], "title": "Воспитание детей гей-парами", "subtitle": "Ситуация", "questions": ["Как вы относитесь к такому усыновлению?", "Имеет ли значение пол ребёнка?", "Имеет ли значение пол усыновителей?"], "description": "Ваше отношение к усыновлению детей gay-парами"}');
INSERT INTO topic VALUES (5, '{"body": "Полиамория (др.-греч. πολύς — многочисленный и лат. amor — любовь) — система этических взглядов на любовь, допускающая возможность существования множественных любовных отношений у одного человека с несколькими людьми одновременно, с согласия и одобрения всех участников этих отношений. Полиаморией также называют практику любовных отношений, воплощающую эти взгляды в действительности.", "refs": ["https://ru.wikipedia.org/wiki/%D0%9F%D0%BE%D0%BB%D0%B8%D0%B0%D0%BC%D0%BE%D1%80%D0%B8%D1%8F"], "title": "Полиамория", "subtitle": "Понятие", "questions": ["Каково ваше отношение к полиамории?", "Допускаете ли лично вы возможность любить нескольких людей одновременно? Почему?", "Как бы вы отнеслись если бы ваш любимый человек любил ещё кого-то кроме вас?"], "description": "Любовь ко многим людям одновременно"}');
INSERT INTO topic VALUES (4, '{"body": "Новые компьютеры на платформах Intel (и AMD) содержат \"заднюю дверь\" - Intel Management Engine, позволяющую узкому кругу лиц полностью и тайно брать его под контроль: видеть изображение на экране, получать доступ к файлам и периферийным устройствам даже когда компьютер выключен.", "refs": ["https://libreboot.org/faq.html#intel", "http://www.techrepublic.com/article/is-the-intel-management-engine-a-backdoor/", "https://hackaday.com/2016/01/22/the-trouble-with-intels-management-engine/"], "title": "Приватность и анонимность", "subtitle": "Факт", "questions": ["Как вы к этому относитесь?", "Нужна ли приватность порядочным людям не нарушающим законы?"], "description": "Ваш компьютер просматривается и прослушивается"}');


--
-- TOC entry 2142 (class 0 OID 0)
-- Dependencies: 186
-- Name: topic_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('topic_id_seq', 10, true);


--
-- TOC entry 2014 (class 2606 OID 16395)
-- Name: topic topic_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY topic
    ADD CONSTRAINT topic_pkey PRIMARY KEY (id);


-- Completed on 2017-08-18 18:51:56 CEST

--
-- PostgreSQL database dump complete
--

