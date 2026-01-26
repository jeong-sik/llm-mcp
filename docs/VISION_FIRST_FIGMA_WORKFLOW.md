# Vision-First Figma Workflow (Top-Node → Detail)

Figma 전체 캔버스를 그대로 구현하지 말고 **상위 노드부터 선택 → 필요한 부모만 남김 → 디테일로 내려가는** 워크플로우.

## 목표
- “쓰레기 제거” 후 **필요한 상위 프레임만** 구현
- Vision(이미지) 기준으로 구조/우선순위를 정한 뒤 디테일 작업
- 검증은 **스크린샷 비교 + 텍스트 매칭**으로 진행

---

## Phase 0 — 상위 노드 추출

**1) 노드 검색(라벨 기반)**
```
figma_search query="to-be"
figma_search query="as-is"
figma_search query="팝업"
```

**2) 후보 프레임만 고름**
- 크기: 메인 화면(예: 900x800+), 팝업(예: 360~520px)
- 라벨: `to-be_...`, `as-is_...`, `...팝업...`
- 컨텍스트: 실제 작업 범위와 일치하는 것만 선택

**3) 필요 없는 프레임 제거**
- 설명용 텍스트, 히스토리, 참고 스크린 등은 제외

---

## Phase 1 — Vision 기준 상위 프레임 확보

**선택한 상위 프레임만 이미지로 확보**
```
figma_export_image node_id=<FRAME_ID> format=png scale=1
```

> 이 시점에서 Vision 분석은 **상위 구조 파악용**.

---

## Phase 2 — Vision 분석 (구조/우선순위)

체크리스트:
- 레이아웃(그리드/정렬/구역 분리)
- UI 핵심 컴포넌트(탭, 패널, 버튼, 입력행)
- 텍스트 계층(헤더 → 설명 → 힌트)
- 반복되는 패턴(입력행, 버튼 그룹)

결과: **구현 우선순위 목록** 만들기  
예) `탭 > 패널 > 액션 버튼 > 입력행 > 아이콘`

---

## Phase 3 — Skeleton 구현

**먼저 박스만 맞춘다.**  
색상/텍스트 최소화하고 레이아웃만 맞춤.

필수:
- 캔버스 크기 고정
- 핵심 컨테이너 배치 (padding, gap 기준)
- 반복 패턴은 미리 컴포넌트화

---

## Phase 4 — Detail Loop (Component 단위)

컴포넌트별 반복:
1) 단일 컴포넌트만 구현
2) 스크린샷 → 비교
3) 수치/위치/색상 보정

예: 탭 → 패널 → 카드 → 입력행 → 버튼

---

## Phase 5 — 검증 루프

**스크린샷 비교**
- Figma 원본 PNG
- HTML 렌더 PNG

```
figma_verify_visual html_screenshot=... node_id=... target_ssim=0.9
```

**보조 비교**
```
magick compare figma.png render.png -compose src -highlight-color red diff.png
magick figma.png render.png +append side.png
```

---

## Phase 6 — 쓰레기 제거 확인

다음이 남아있으면 제거:
- 설명용 텍스트 블록 (가이드 문장/이슈 메모)
- 다른 flow/버전 프레임
- 로직과 무관한 스크린샷 묶음

---

## Phase 7 — MASC 작업화

**필요한 작업만 태스크로 전환**
- 레이아웃/스타일 정합성
- 버튼/아이콘 상태
- 텍스트 복사/문구 반영
- 입력행 추가/삭제 UX

---

## Practical Notes

- Figma `get_node_summary`가 큰 캔버스에서 timeout 날 경우:
  - `figma_search` + `figma_export_image`로 상위 노드를 먼저 확보
- Playwright/브라우저 렌더링 이슈:
  - arm64 환경에서 headless shell 경로 mismatch 시
    ```
    ln -s <arm64_path> <x64_expected_path>
    ```

---

## 실행 예시 (to-be 프레임 기준)

```
# 프레임 선택
figma_export_image node_id=2089:11127

# Vision-first 구현 → render
playwright screenshot --full-page file:///path/to/vision-first.html out.png

# 비교
figma_verify_visual node_id=2089:11127 html_screenshot=out.png
```

---

## 핵심 원칙

1. **전체 구현 금지** → 상위 노드 선택
2. **Vision 우선** → DSL은 보조
3. **컴포넌트 단위 반복** → SSIM/텍스트 개선
4. **필요한 부모만 유지** → 쓰레기 제거
