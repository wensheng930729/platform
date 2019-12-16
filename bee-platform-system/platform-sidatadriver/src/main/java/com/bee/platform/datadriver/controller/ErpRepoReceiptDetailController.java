package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpRepoReceiptDetailProductOutDTO;
import com.bee.platform.datadriver.dto.ErpRepoReceiptDetailRawInDTO;
import com.bee.platform.datadriver.dto.RepoReceiptDetailDTO;
import com.bee.platform.datadriver.rq.ReceiptDetailRQ;
import com.bee.platform.datadriver.rq.RepoReceiptDetailProductOutRQ;
import com.bee.platform.datadriver.rq.RepoReceiptDetailRawInRQ;
import com.bee.platform.datadriver.service.ErpRepoReceiptDetailService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 仓库单明细 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpRepoReceiptDetail", tags = "原料入库(采购收货)或成品出库(销售发货)明细相关接口")
@RequestMapping("/erpRepoReceiptDetail")
public class ErpRepoReceiptDetailController {

    @Autowired
    private ErpRepoReceiptDetailService receiptDetailService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/listRepoReceiptDetailByOrder")
    @ApiOperation(value = "根据订单id查询收发货列表")
    public ResponseResult<List<RepoReceiptDetailDTO>> listRepoReceiptDetailByOrder(HttpServletRequest request, @RequestBody ReceiptDetailRQ rq){
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,receiptDetailService.listRepoReceiptDetail(rq));
    }

    @ApiOperation(value = "保存原料入库明细单（采购收货）")
    @PostMapping("/saveRepoReceiptDetailRawIn")
    public ResponseResult<Integer> saveRepoReceiptDetailRawIn(HttpServletRequest request, @RequestBody @Valid RepoReceiptDetailRawInRQ rq) {
        if(ObjectUtils.isEmpty(rq)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = receiptDetailService.saveRepoReceiptDetailRawIn(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }


    @ApiOperation(value = "根据id删除原料入库明细(采购收货)")
    @DeleteMapping("/deleteRepoReceiptDetailRawIn/{id}")
    public ResponseResult<Integer> deleteRepoReceiptDetailRawInById(HttpServletRequest request, @PathVariable Integer id){
        if(ObjectUtils.isEmpty(id)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        receiptDetailService.deleteRepoReceiptDetailRawIn(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }


    @ApiOperation(value = "保存成品出库明细单(销售发货)")
    @PostMapping("/saveRepoReceiptDetailProductOut")
    public ResponseResult<Integer> saveRepoReceiptDetailProductOut(HttpServletRequest request, @RequestBody @Valid RepoReceiptDetailProductOutRQ rq) {
        if(ObjectUtils.isEmpty(rq)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = receiptDetailService.saveRepoReceiptDetailProductOut(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }


    @ApiOperation(value = "根据id删除成品出库明细(销售发货)")
    @DeleteMapping("/deleteRepoReceiptDetailProductOut/{id}")
    public ResponseResult<Integer> deleteRepoReceiptDetailProductOutById(HttpServletRequest request, @PathVariable Integer id){
        if(ObjectUtils.isEmpty(id)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        receiptDetailService.deleteRepoReceiptDetailProductOutById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }




    @ApiOperation(value = "根据id查询原料入库(采购收货)详情")
    @GetMapping("getRepoReceiptDetailRawInById")
    public ResponseResult<ErpRepoReceiptDetailRawInDTO> getRepoReceiptDetailRawInById(@RequestParam Integer id){
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpRepoReceiptDetailRawInDTO dto = receiptDetailService.getRepoReceiptDetailRawInById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }

    @ApiOperation(value = "根据id查询成品出库(销售发货)详情")
    @GetMapping("getRepoReceiptDetailProductOutById")
    public ResponseResult<ErpRepoReceiptDetailProductOutDTO> getRepoReceiptDetailProductOutById(@RequestParam Integer id){
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpRepoReceiptDetailProductOutDTO dto = receiptDetailService.getRepoReceiptDetailProductOutById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }




}

