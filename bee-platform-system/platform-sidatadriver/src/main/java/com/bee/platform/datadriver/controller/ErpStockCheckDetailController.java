package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpStockCheckDetailDTO;
import com.bee.platform.datadriver.rq.ErpStockCheckDetailRQ;
import com.bee.platform.datadriver.service.ErpStockCheckDetailService;
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
 * 库存盘点明细 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpStockCheckDetail", tags = "库存盘点明细相关接口")
@RequestMapping("/erpStockCheckDetail")
public class ErpStockCheckDetailController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpStockCheckDetailService erpStockCheckDetailService;


    @ApiOperation(value = "保存库存盘点明细单")
    @PostMapping("/saveStockCheckDetail")
    public ResponseResult<Integer> saveStockCheckDetail(HttpServletRequest request, @RequestBody @Valid ErpStockCheckDetailRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = erpStockCheckDetailService.saveStockCheckDetail(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据id删除库存盘点明细")
    @DeleteMapping("/deleteStockCheckDetailById/{id}")
    public ResponseResult<Integer> deleteStockCheckDetailById(HttpServletRequest request, @PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        erpStockCheckDetailService.deleteStockCheckDetailById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据id查询库存盘点详情")
    @GetMapping("getStockCheckDetailById")
    public ResponseResult<ErpStockCheckDetailDTO> getStockCheckDetailById(@RequestParam Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpStockCheckDetailDTO dto = erpStockCheckDetailService.getStockCheckDetailById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

