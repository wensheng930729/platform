package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderDetailDTO;
import com.bee.platform.datadriver.rq.ErpOutOfStockOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpOutOfStockOrderDetailService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

/**
 * <p>
 * 领料出库明细表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpOutOfStockOrderDetail", tags = "领料出库明细相关接口")
@RequestMapping("/erpOutOfStockOrderDetail")
public class ErpOutOfStockOrderDetailController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpOutOfStockOrderDetailService erpOutOfStockOrderDetailService;


    @ApiOperation(value = "保存领料出库详情信息单")
    @PostMapping("/saveOutOfStockOrderDetail")
    public ResponseResult<Integer> saveOutOfStockOrderDetail(HttpServletRequest request, @RequestBody @Valid ErpOutOfStockOrderDetailRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = erpOutOfStockOrderDetailService.saveOutOfStockOrderDetail(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据id删除领料出库详情")
    @DeleteMapping("/deleteOutOfStockOrderDetailById/{id}")
    public ResponseResult<Integer> deleteOutOfStockOrderDetailById(HttpServletRequest request, @PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        erpOutOfStockOrderDetailService.deleteOutOfStockOrderDetailById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据id查询领料出库详情")
    @GetMapping("getOutOfStockOrderDetailById")
    public ResponseResult<ErpOutOfStockOrderDetailDTO> getOutOfStockOrderDetailById(@RequestParam Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpOutOfStockOrderDetailDTO dto = erpOutOfStockOrderDetailService.getOutOfStockOrderDetailById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

