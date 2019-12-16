package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderDTO;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderSearchListDTO;
import com.bee.platform.datadriver.dto.ErpOpeningInventorySearchDTO;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrder;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpOpeningInventoryOrderSaveRQ;
import com.bee.platform.datadriver.rq.ErpOpeningInventorySearchRQ;
import com.bee.platform.datadriver.rq.OpeningInventoryOrderQueryRQ;
import com.bee.platform.datadriver.service.ErpOpeningInventoryOrderService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 期初库存主表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpOpeningInventoryOrder", tags = "期初库存主表相关接口")
@RequestMapping("/erpOpeningInventoryOrder")
public class ErpOpeningInventoryOrderController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpOpeningInventoryOrderService openingInventoryOrderService;

    @PostMapping("/searchOpeningInventoryOrderByCondition")
    @ApiOperation(value = "条件查询期初库存列表")
    @ApiIgnore
    public ResponseResult<List<ErpOpeningInventoryOrderSearchListDTO>> searchOpeningInventoryOrderByCondition(HttpServletRequest request, @RequestBody OpeningInventoryOrderQueryRQ rq, Page page) {

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        String sysToken = request.getHeader("sysToken");
        return openingInventoryOrderService.searchOpeningInventoryOrderByCondition(userInfo, rq, page, sysToken);
    }


    @PostMapping("/searchOpeningInventoryByCondition")
    @ApiOperation(value = "条件查询期初库存主表信息列表")
    public ResponseResult<List<ErpOpeningInventorySearchDTO>> searchOpeningInventoryByCondition(HttpServletRequest request, @RequestBody ErpOpeningInventorySearchRQ rq, Page page) {

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return openingInventoryOrderService.searchOpeningInventoryByCondition(rq, page, companyId);
    }






    @ApiOperation(value = "根据id查看期初库存详情")
    @GetMapping("/getOpeningInventoryById/{id}")
    public ResponseResult<ErpOpeningInventoryOrderDTO> getOpeningInventoryById(@PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpOpeningInventoryOrderDTO dto = openingInventoryOrderService.getOpeningInventoryById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }

    @ApiOperation(value = "获取期初库存信息")
    @GetMapping("/getOpeningInventoryOrder")
    @ApiIgnore
    public ResponseResult<ErpOpeningInventoryOrder> getOpeningInventoryOrder(HttpServletRequest request, String id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return openingInventoryOrderService.getOpeningInventoryOrder(userInfo, id);
    }

    @DeleteMapping("/deleteOpeningInventoryOrderById")
    @Log(businessType = EnumBusinessType.INIT_STOCK, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除期初库存信息")
    public ResponseResult<Integer> deleteOpeningInventoryOrderById(HttpServletRequest request, @RequestParam Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        openingInventoryOrderService.deleteOpeningInventoryOrderById(userInfo, id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @ApiOperation(value = "修改期初库存状态")
    @Log(businessType = EnumBusinessType.INIT_STOCK, operateType = OperateType.EDIT)
    @PutMapping("/updateOpeningInventoryState")
    public ResponseResult<Integer> updateOpeningInventoryState(HttpServletRequest request, @RequestParam Integer id, @RequestParam Integer state) {

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        openingInventoryOrderService.updateOpeningInventoryState(userInfo, id, state);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @GetMapping("/confirmOpeningInventoryOrder")
    @Log(businessType = EnumBusinessType.INIT_STOCK, operateType = OperateType.EDIT)
    @ApiOperation(value = "确认期初库存信息")
    @ApiIgnore
    public ResponseResult<Integer> confirmOpeningInventoryOrder(HttpServletRequest request, String id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return openingInventoryOrderService.confirmOpeningInventoryOrder(userInfo, id);
    }

    @PostMapping("/saveOpeningInventoryOrder")
    @Log(businessType = EnumBusinessType.INIT_STOCK, operateType = OperateType.ADD)
    @ApiOperation(value = "保存期初库存")
    public ResponseResult<Integer> saveOpeningInventoryOrder(HttpServletRequest request, @RequestBody @Valid ErpOpeningInventoryOrderSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        Integer id = openingInventoryOrderService.saveOpeningInventoryOrder(userInfo, rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


}

