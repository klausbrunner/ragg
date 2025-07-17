#pragma once

#include <webp/encode.h>
#include <webp/mux.h>

#include <cstring>
#include <vector>
#include <memory>

#include "AggDevice.h"
#include "files.h"
#include "ragg.h"

using WebPPicturePtr = std::unique_ptr<WebPPicture, void(*)(WebPPicture*)>;
using WebPDataPtr = std::unique_ptr<WebPData, void(*)(WebPData*)>;
using FilePtr = std::unique_ptr<FILE, int(*)(FILE*)>;

inline WebPPicturePtr make_webp_picture() {
  auto pic = std::make_unique<WebPPicture>();
  if (!WebPPictureInit(pic.get())) {
    throw std::runtime_error("WebPPictureInit failed");
  }
  return WebPPicturePtr(pic.release(), WebPPictureFree);
}

inline WebPDataPtr make_webp_data() {
  auto data = std::make_unique<WebPData>();
  std::memset(data.get(), 0, sizeof(WebPData));
  return WebPDataPtr(data.release(), WebPDataClear);
}

inline FilePtr make_file(const char* filename, const char* mode) {
  FILE* f = unicode_fopen(filename, mode);
  if (!f) {
    throw std::runtime_error("Failed to open file");
  }
  return FilePtr(f, fclose);
}

// A variant of AggDeviceWebP that captures each rendered page to memory,
// encodes it as a WebP frame, and then uses libwebp's mux API to assemble
// them into a single animated WebP on close().
template <class PIXFMT>
class AggDeviceWebPAnim : public AggDevice<PIXFMT> {
 public:
  AggDeviceWebPAnim(const char* fp, int w, int h, double pointsize,
                    int background, double res, double scaling, bool snap_rect,
                    bool lossy, int quality, int delay_ms, int loop_count)
      : AggDevice<PIXFMT>(fp, w, h, pointsize, background, res, scaling,
                          snap_rect),
        lossy_(lossy),
        quality_(quality),
        delay_ms_(delay_ms),
        loop_count_(loop_count),
        mux_(WebPMuxNew(), WebPMuxDelete) {
    if (!mux_) {
      throw std::runtime_error("Failed to create WebP mux");
    }
    
    WebPMuxAnimParams params;
    std::memset(&params, 0, sizeof(params));
    params.bgcolor = 0;  // Background color (RGB or RGBA)
    params.loop_count = loop_count_;
    
    if (WebPMuxSetAnimationParams(mux_.get(), &params) != WEBP_MUX_OK) {
      throw std::runtime_error("Failed to set WebP animation parameters");
    }
  }

  bool savePage() {
    AggDevice<PIXFMT>::savePage();
    demultiply<PIXFMT>(this->pixf);

    try {
      auto pic = make_webp_picture();
      
      WebPMemoryWriter wr;
      WebPMemoryWriterInit(&wr);
      auto cleanup_wr = [&wr]() { WebPMemoryWriterClear(&wr); };
      auto wr_guard = std::unique_ptr<WebPMemoryWriter, decltype(cleanup_wr)>(&wr, cleanup_wr);

      pic->width = this->width;
      pic->height = this->height;
      pic->writer = WebPMemoryWrite;
      pic->custom_ptr = &wr;

      WebPConfig config;
      if (!WebPConfigInit(&config)) {
        return false;
      }
      config.quality = float(quality_);
      config.lossless = lossy_ ? 0 : 1;

      const int stride = this->rbuf.stride_abs();
      const auto importer = (PIXFMT::num_components == 3) ? WebPPictureImportRGB
                                                          : WebPPictureImportRGBA;

      if (!importer(pic.get(), reinterpret_cast<const uint8_t*>(this->buffer),
                    stride)) {
        Rf_warning("WebPPictureImport failed: error code %d", pic->error_code);
        return false;
      }

      if (!WebPEncode(&config, pic.get())) {
        Rf_warning("WebPEncode failed: error code %d", pic->error_code);
        return false;
      }

      // Save in memory for muxing later
      frames_.emplace_back(wr.mem, wr.mem + wr.size);
      return true;
      
    } catch (const std::exception& e) {
      Rf_warning("Exception in savePage: %s", e.what());
      return false;
    }
  }

  void close() {
    AggDevice<PIXFMT>::savePage();

    for (const auto& raw : frames_) {
      WebPMuxFrameInfo finfo;
      std::memset(&finfo, 0, sizeof(finfo));
      finfo.bitstream.bytes = raw.data();
      finfo.bitstream.size = raw.size();
      finfo.duration = delay_ms_;
      finfo.id = WEBP_CHUNK_ANMF;
      finfo.x_offset = 0;
      finfo.y_offset = 0;
      finfo.dispose_method = WEBP_MUX_DISPOSE_NONE;
      finfo.blend_method = WEBP_MUX_BLEND;
      if (WebPMuxPushFrame(mux_.get(), &finfo, 1) != WEBP_MUX_OK) {
        Rf_error("Failed to push WebP frame to mux");
      }
    }

    try {
      auto out = make_webp_data();
      if (WebPMuxAssemble(mux_.get(), out.get()) != WEBP_MUX_OK) {
        Rf_error("WebP mux assemble failed");
      }

      auto fd = make_file(this->file.c_str(), "wb");
      if (fwrite(out->bytes, 1, out->size, fd.get()) != out->size) {
        Rf_error("Failed to write WebP animation file");
      }
      
    } catch (...) {
      AggDevice<PIXFMT>::close();
      throw;
    }

    AggDevice<PIXFMT>::close();
  }

 private:
  const bool lossy_;
  const int quality_;
  const int delay_ms_;
  const int loop_count_;
  std::unique_ptr<WebPMux, void(*)(WebPMux*)> mux_;
  std::vector<std::vector<uint8_t>> frames_;
};

template class AggDeviceWebPAnim<pixfmt_type_24>;
template class AggDeviceWebPAnim<pixfmt_type_32>;