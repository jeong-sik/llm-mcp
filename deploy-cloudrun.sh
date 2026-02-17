#!/bin/bash
# LLM-MCP Cloud Run 배포 스크립트

set -e

# 환경 변수 설정
PROJECT_ID="${PROJECT_ID:-kidsnote-ax-tf}"  # GCP Project ID
REGION="${REGION:-asia-northeast3}"          # Cloud Run 지역
SERVICE_NAME="${SERVICE_NAME:-llm-mcp}"      # 서비스 이름
IMAGE_NAME="${IMAGE_NAME:-llm-mcp}"         # 이미지 이름
PORT="${PORT:-8932}"                       # 포트 번호

# 색상 출력을 위한 함수
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 에러 체크
check_prerequisites() {
    echo -e "${YELLOW}필수 요소 확인 중...${NC}"

    if ! command -v gcloud &> /dev/null; then
        echo -e "${RED}Error: gcloud CLI가 설치되어 있지 않습니다.${NC}"
        exit 1
    fi

    if ! command -v docker &> /dev/null; then
        echo -e "${RED}Error: Docker가 설치되어 있지 않습니다.${NC}"
        exit 1
    fi

    echo -e "${GREEN}필수 요소 확인 완료${NC}"
}

# Docker 이미지 빌드 및 푸시
build_and_push_image() {
    echo -e "${YELLOW}Docker 이미지 빌드 중...${NC}"

    # Google Cloud Docker 레지스트리 설정
    gcloud auth configure-docker "${REGION}-docker.pkg.dev"

    # 이미지 이름 설정
    IMAGE_URI="${REGION}-docker.pkg.dev/${PROJECT_ID}/${IMAGE_NAME}:${SERVICE_NAME}-${GITHUB_SHA:-$(date +%Y%m%d-%H%M%S)}"

    # Docker 이미지 빌드
    docker build -t "${IMAGE_URI}" .

    # Docker 이미지 푸시
    docker push "${IMAGE_URI}"

    echo -e "${GREEN}Docker 이미지 빌드 및 푸시 완료: ${IMAGE_URI}${NC}"

    echo "${IMAGE_URI}" > image_uri.txt
}

# Cloud Run 배포
deploy_to_cloudrun() {
    local image_uri=$(cat image_uri.txt)

    echo -e "${YELLOW}Cloud Run 배포 중...${NC}"

    # Cloud Run 배포 명령어
    gcloud run deploy "${SERVICE_NAME}" \
        --image "${image_uri}" \
        --region "${REGION}" \
        --platform managed \
        --ingress=all \
        --allow-unauthenticated \
        --port "${PORT}" \
        --memory 1Gi \
        --cpu 1 \
        --min-instances 0 \
        --max-instances 10 \
        --concurrency 80 \
        --timeout 300 \
        --set-env-vars "LLM_MCP_PUBLIC=1,LLM_MCP_CORS_MODE=permissive,LLM_MCP_API_KEY=${LLM_MCP_API_KEY}" \
        --set-secrets "LLM_MCP_API_KEY=llm-mcp-api-key:latest" \
        --no-allow-unauthenticated \
        --quiet

    echo -e "${GREEN}Cloud Run 배포 완료${NC}"
}

# 서비스 URL 가져오기
get_service_url() {
    echo -e "${YELLOW}서비스 URL 가져오는 중...${NC}"

    SERVICE_URL=$(gcloud run services describe "${SERVICE_NAME}" \
        --region "${REGION}" \
        --format 'value(status.url)')

    if [ -n "${SERVICE_URL}" ]; then
        echo -e "${GREEN}서비스 URL: ${SERVICE_URL}${NC}"
        echo "${SERVICE_URL}" > service_url.txt
    else
        echo -e "${RED}서비스 URL을 가져오지 못했습니다${NC}"
        exit 1
    fi
}

# 메인 실행
main() {
    echo -e "${GREEN}LLM-MCP Cloud Run 배포를 시작합니다.${NC}"
    echo -e "${YELLOW}Project ID: ${PROJECT_ID}${NC}"
    echo -e "${YELLOW}Region: ${REGION}${NC}"
    echo -e "${YELLOW}Service Name: ${SERVICE_NAME}${NC}"
    echo "----------------------------------------"

    check_prerequisites
    build_and_push_image
    deploy_to_cloudrun
    get_service_url

    echo "----------------------------------------"
    echo -e "${GREEN}배포가 성공적으로 완료되었습니다!${NC}"

    # 서비스 URL 출력
    if [ -f "service_url.txt" ]; then
        echo -e "\n${YELLOW}서비스 접속 URL:${NC}"
        cat service_url.txt
    fi
}

# 스크립트 실행
main "$@"