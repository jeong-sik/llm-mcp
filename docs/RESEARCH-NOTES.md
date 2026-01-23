# Compact Protocol 논문화 연구 조사

> Created: 2026-01-12
> Status: Research phase

## 핵심 논문들

### 1. LLMLingua 시리즈 (Microsoft)

| 논문 | 학회 | 핵심 내용 | 링크 |
|------|------|----------|------|
| LLMLingua | EMNLP'23 | 프롬프트 20x 압축, 성능 손실 최소화 | [arXiv](https://arxiv.org/abs/2310.05736) |
| LongLLMLingua | ACL'24 | Long context "lost in middle" 해결, 토큰 1/4 사용 | [arXiv](https://arxiv.org/abs/2310.06839) |
| LLMLingua-2 | ACL'24 | GPT-4 distillation, 3-6x 더 빠름 | [arXiv](https://arxiv.org/abs/2403.12968) |

- GitHub: https://github.com/microsoft/LLMLingua

### 2. Language Modeling Is Compression (ICLR 2024)

- LLM을 압축기로 활용
- Chinchilla 70B가 ImageNet에서 PNG(58.5%)보다 효율적(43.4%)
- [arXiv](https://arxiv.org/abs/2309.10668)

### 3. 2025 최신 연구

| 논문 | 핵심 내용 | 링크 |
|------|----------|------|
| Vision-centric Token Compression (Vist) | 이미지 토큰 2.3x 감소, FLOPs 16% ↓ | [arXiv](https://arxiv.org/abs/2502.00791) |
| SelfBudgeter | 적응적 토큰 버짓 할당 | [arXiv](https://arxiv.org/pdf/2505.11274) |
| Acon | Agent context 최적화, 메모리 26-54% 감소 | [arXiv](https://arxiv.org/html/2510.00615v1) |
| Prompt Compression Survey | 종합 서베이 | [arXiv](https://arxiv.org/html/2410.12388v2) |

---

## HN/GeekNews 트렌드

### Hacker News

| 주제 | 핵심 인사이트 | 링크 |
|------|-------------|------|
| LLM으로 텍스트 압축 | 토큰 확률 분포 + 엔트로피 압축 (Huffman/ANS) | [HN](https://news.ycombinator.com/item?id=40245261) |
| Un-LOCC | 텍스트→이미지 압축으로 90% 비용 절감 | [HN](https://news.ycombinator.com/item?id=45820573) |
| HTML 구조 압축 | DOM 구조 보존하며 90% 비용 절감 | [HN](https://news.ycombinator.com/item?id=42850979) |
| Prompt-refiner | 제로 의존성, ~15% 토큰 절약 | [HN](https://news.ycombinator.com/item?id=46306585) |
| MessagePack | JSON 대비 30-40% 작음 | [HN](https://news.ycombinator.com/item?id=42663047) |

### GeekNews (한국어)

- [Microsoft LLMLingua](https://news.hada.io/topic?id=12470) - 프롬프트 압축 20배
- [컨텍스트 변질 연구](https://news.hada.io/topic?id=22013) - 긴 입력의 성능 저하 현상
- [Token Cost 계산기](https://news.hada.io/topic?id=15421) - 400+ LLM 비용 비교

---

## Compact Protocol 차별점

| 기존 연구 | 우리 연구 |
|-----------|-----------|
| 프롬프트 의미 압축 | **와이어 레벨** 바이너리 압축 |
| 단방향 (입력 OR 출력) | **양방향** 압축 |
| 범용 직렬화 | **LLM 응답 특화** 포맷 |
| 정적 포맷 | **적응형** 포맷 선택 |
| 배치 처리 | **스트리밍 델타** 지원 |

### 빈틈 발견

1. 대부분 연구는 **입력(프롬프트) 압축**에 집중
2. **출력(응답) 압축**은 거의 연구 안 됨
3. **MCP 레이어에서의 응답 압축**은 우리가 유일

---

## 논문화 방향

### 제목 후보

1. "Compact Protocol: A Bidirectional Wire-Level Compression Framework for LLM API Communication"
2. "Beyond Prompt Compression: Wire-Level Optimization for Multi-Agent LLM Systems"
3. "Adaptive Response Encoding for Token-Efficient LLM Communication"

### 제출 타겟

| Venue | 마감 | 적합도 |
|-------|------|--------|
| arXiv preprint | 언제든 | ⭐⭐⭐⭐⭐ |
| NeurIPS 2026 Workshop | ~Sep 2026 | ⭐⭐⭐⭐ |
| EMNLP 2026 Industry | ~Jun 2026 | ⭐⭐⭐⭐ |
| ACL 2026 System Demo | ~Feb 2026 | ⭐⭐⭐ |

### 핵심 기여 (Contributions)

1. **Bidirectional Compression**: 입력 + 출력 모두 압축
2. **Adaptive Format Selection**: 응답 크기에 따른 자동 포맷 선택
3. **Delta Streaming Protocol**: 실시간 업데이트용 증분 인코딩
4. **MCP Integration**: Anthropic MCP 생태계 통합
5. **Open-source Implementation**: OCaml + Python/TypeScript 클라이언트

---

## 다음 단계

### Phase 1: 입력 압축 확장 (1-2주)
- [ ] Tool definition caching 구현
- [ ] Conversation diff encoding
- [ ] System prompt hashing

### Phase 2: 벤치마크 (1주)
- [ ] 토큰 절약률 측정
- [ ] 레이턴시 영향 분석
- [ ] 압축률 vs 품질 트레이드오프

### Phase 3: 논문 작성 (2-3주)
- [ ] Related work 섹션
- [ ] 실험 결과 정리
- [ ] arXiv 제출

---

## 참고 자료

- [Anthropic MCP Introduction](https://www.anthropic.com/news/model-context-protocol)
- [MCP Code Execution](https://www.anthropic.com/engineering/code-execution-with-mcp)
- [LiteLLM](https://github.com/BerriAI/litellm) - 100+ LLM API 통합
