package com.bee.platform.dinas.datadriver.hystrix;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.service.feign.DinasDatadriverBeatFeignClient;

import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class DinasDatadriverBeatFeignClientFallbackFactory implements FallbackFactory<DinasDatadriverBeatFeignClient> {

	@Override
	public DinasDatadriverBeatFeignClient create(Throwable cause) {

		return new DinasDatadriverBeatFeignClient() {

			@Override
			public ResponseResult<String> beat() {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, ResCodeEnum.HYSTRIX_ENABLED.msg);
			}

		};
	}

}
