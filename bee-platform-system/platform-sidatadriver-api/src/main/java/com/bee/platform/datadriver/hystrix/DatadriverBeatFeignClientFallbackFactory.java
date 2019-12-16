package com.bee.platform.datadriver.hystrix;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.service.feign.DatadriverBeatFeignClient;

import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class DatadriverBeatFeignClientFallbackFactory implements FallbackFactory<DatadriverBeatFeignClient> {

	@Override
	public DatadriverBeatFeignClient create(Throwable cause) {

		return new DatadriverBeatFeignClient() {

			@Override
			public ResponseResult<String> beat() {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, ResCodeEnum.HYSTRIX_ENABLED.msg);
			}

		};
	}

}
