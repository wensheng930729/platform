package com.bee.platform.dinas.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerBoxDTO;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerDetailDTO;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerListDTO;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerAddRQ;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerUpdateRQ;
import com.bee.platform.dinas.datadriver.service.DinasCustomerService;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 客户表 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@RestController
@RequestMapping("/dinasCustomer")
@Api(value = "DinasCustomerController", tags = "砂石客户相关接口")
public class DinasCustomerController {

    @Autowired
    private DinasCustomerService customerService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/add")
    @ApiOperation(value = "增加")
    public ResponseResult addCustomer(@RequestHeader("sysToken") String sysToken, @Valid @RequestBody DinasCustomerAddRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerService.addCustomer(userInfo, rq);
    }

    @PostMapping("/delete/{type}")
    @ApiOperation(value = "删除")
    public ResponseResult deleteCustomer(@RequestHeader("sysToken") String sysToken, @PathVariable("type") Integer type, @RequestBody List<Integer> ids) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (CollectionUtils.isEmpty(ids)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_SELECT_RECORD);
        }
        return customerService.deleteCustomer(userInfo, type, ids);
    }

    @PostMapping("/update")
    @ApiOperation(value = "编辑")
    public ResponseResult updateCustomer(@RequestHeader("sysToken") String sysToken, @Valid @RequestBody DinasCustomerUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerService.updateCustomer(userInfo, rq);
    }

    @GetMapping("/switch/{id}/{status}")
    @ApiOperation(value = "切换")
    public ResponseResult switchCustomer(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id, @PathVariable("status") Integer status) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerService.switchCustomer(userInfo, id, status);
    }

    @GetMapping("/detail/{id}")
    @ApiOperation(value = "详情")
    public ResponseResult<DinasCustomerDetailDTO> getCustomerDetail(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerService.getCustomerDetail(id);
    }

    @PostMapping("/list")
    @ApiOperation(value = "列表")
    public ResponseResult<List<DinasCustomerListDTO>> getCustomerList(@RequestHeader("sysToken") String sysToken, Page page, @Valid @RequestBody DinasCustomerQueryRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerService.getCustomerList(page, userInfo.getOrgId(), rq);
    }

    @GetMapping("/box/{type}")
    @ApiOperation(value = "下拉框-根据类型查询客户")
    public ResponseResult<List<DinasCustomerBoxDTO>> getCustomerByType(@RequestHeader("sysToken") String sysToken, @PathVariable("type") Integer type) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer orgId = userInfo.getOrgId();
        return customerService.getCustomerByType(orgId, type);
    }

    @GetMapping("/box/{type}/{productId}")
    @ApiOperation(value = "下拉框--根据类型和产品查询客户")
    public ResponseResult<List<DinasCustomerBoxDTO>> getCustomerByTypeAndProduct(@RequestHeader("sysToken") String sysToken,
                                                                                 @PathVariable("productId") Integer productId,
                                                                                 @PathVariable("type") Integer type) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer orgId = userInfo.getOrgId();
        return customerService.getCustomerByTypeAndProduct(orgId, productId, type);
    }

}

