SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';
SET search_path = public, pg_catalog;
SET default_tablespace = '';
SET default_with_oids = false;

CREATE TABLE IF NOT EXISTS topic (
    id integer NOT NULL,
    content jsonb NOT NULL
);

ALTER TABLE topic OWNER TO postgres;

CREATE SEQUENCE IF NOT EXISTS topic_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE topic_id_seq OWNER TO postgres;
ALTER SEQUENCE topic_id_seq OWNED BY topic.id;
ALTER TABLE ONLY topic ALTER COLUMN id SET DEFAULT nextval('topic_id_seq'::regclass);

INSERT INTO topic VALUES (1, '{"full_text": "Вашей женщине её коллега/знакомый мужчина намеревается подарить ювелирное украшение", "title": "Дорогие подарки", "questions": ["Как отреагировать вашей женщине?", "Справедливы ли ваши рассуждения в обратной ситуации когда подарок дарит женшина мужчине?"], "short_text": "О том, что подарки не всегда являются безобидными"}');
INSERT INTO topic VALUES (2, '{"full_text": "Вам нравится творчество какого-нибудь музыканта или киноартиста, но его политические взгляды или личные позиции по иным, принципиальным для вас вопросам противоречат вашим.", "title": "Артисты и их политические взгляды", "questions": ["будете ли вы наслаждаться творчеством этого человека, если оно непосредственно не затрагивает принципиальные различия?"], "short_text": "О целостном восприятии артистов вне зависимости от их политических взглядов"}');
INSERT INTO topic VALUES (3, '{"full_text": "Вы узнаёте, что два человека одинакового пола (gay-пара) усыновили и воспитывают ребёнка", "title": "Воспитание детей гей-парами", "questions": ["Как вы относитесь к такому усыновлению?", "Имеет ли значение пол ребёнка?", "Имеет ли значение пол усыновителей?"], "short_text": "Ваше отношение к усыновлению детей gay-парами"}');
INSERT INTO topic VALUES (4, '{"full_text": "Полиамория (др.-греч. πολύς — многочисленный и лат. amor — любовь) — система этических взглядов на любовь, допускающая возможность существования множественных любовных отношений у одного человека с несколькими людьми одновременно, с согласия и одобрения всех участников этих отношений. Полиаморией также называют практику любовных отношений, воплощающую эти взгляды в действительности.", "title": "Полиамория", "questions": ["Каково ваше отношение к полиамории?", "Допускаете ли лично вы возможность любить нескольких людей одновременно? Почему?", "Как бы вы отнеслись если бы ваш любимый человек любил ещё кого-то кроме вас?"], "short_text": "Любовь ко многим людям одновременно"}');
INSERT INTO topic VALUES (5, '{"full_text": "Новые компьютеры на платформах Intel (и AMD) содержат \"заднюю дверь\" - Intel Management Engine, позволяющую узкому кругу лиц полностью и тайно брать его под контроль: видеть изображение на экране, получать доступ к файлам и периферийным устройствам даже когда компьютер выключен.", "title": "Приватность и анонимность", "questions": ["Как вы к этому относитесь?", "Нужна ли приватность порядочным людям не нарушающим законы?"], "short_text": "Ваш компьютер просматривается и прослушивается"}');


-- ALTER TABLE ONLY topic ADD CONSTRAINT topic_pkey PRIMARY KEY (id);
