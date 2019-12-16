package com.bee.platform.user.hystrix;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthPlatformUserFeignDTO;
import com.bee.platform.user.rq.UserAuthValidateRQ;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
public class AuthUserFeignClientFallbackFactory implements FallbackFactory<AuthUserFeignClient> {

    @Override
    public AuthUserFeignClient create(Throwable cause) {

        return new AuthUserFeignClient() {
            @Override
            public ResponseResult<AuthPlatformUserInfo> simpleUserInfo(String sysToken) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED);
            }

            @Override
            public ResponseResult<AuthPlatformUserInfo> userRoleInfo(String sysToken) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED);
            }

            @Override
            public ResponseResult<List<AuthPlatformUserFeignDTO>> getMoreUserInfo(List<Integer> ids) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, new ArrayList<>(0));
            }

            @Override
            public ResponseResult<Boolean> validate(UserAuthValidateRQ rq) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED);
            }

            @Override
            public ResponseResult<Set<Integer>> subordinates(Integer orgId, Integer userId) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, new HashSet<>(0));
            }
        };
    }

}
