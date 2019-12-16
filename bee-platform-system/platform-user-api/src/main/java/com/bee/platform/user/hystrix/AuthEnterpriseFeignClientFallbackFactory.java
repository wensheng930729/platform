package com.bee.platform.user.hystrix;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class AuthEnterpriseFeignClientFallbackFactory implements FallbackFactory<AuthEnterpriseFeignClient> {

    @Override
    public AuthEnterpriseFeignClient create(Throwable cause) {

        return new AuthEnterpriseFeignClient() {
            @Override
            public ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByUser(String sysToken) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, sysToken);
            }

            @Override
            public ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByCompanyId(Integer companyId) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, null);
            }

            @Override
            public ResponseResult<List<AuthEnterpriseFlatDTO>> getParentSubEnterpriseFlat(String sysToken) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, sysToken);
            }

            @Override
            public ResponseResult getEnterpriseDetail(String sysToken, Integer enterpriseId) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, sysToken);
            }

            @Override
            public ResponseResult getParentEnterpriseId(Integer enterpriseId) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, enterpriseId);
            }

            @Override
            public ResponseResult getAncestor(Integer enterpriseId) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, enterpriseId);
            }

            @Override
            public ResponseResult getEnterpriseUsers(Integer userId) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, userId);
            }

            @Override
            public ResponseResult<List<AuthEnterpriseFeignDetailDTO>> getEnterpriseMoreDetail(List<Integer> orgIds) {
                return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, new ArrayList<>(0));
            }
        };
    }


}
