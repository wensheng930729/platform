package com.bee.platform.dinas.datadriver.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasSaleInvoiceMapper;
import com.bee.platform.dinas.datadriver.dto.DinasUrlDTO;
import com.bee.platform.dinas.datadriver.dto.SaleInvoiceDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleInvoice;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SaleInvoiceListRQ;
import com.bee.platform.dinas.datadriver.rq.SaleInvoiceRQ;
import com.bee.platform.dinas.datadriver.service.DinasSaleInvoiceService;
import com.bee.platform.dinas.datadriver.utils.AppendixUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.*;

/**
 * <p>
 * 销售发票 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasSaleInvoiceServiceImpl extends ServiceImpl<DinasSaleInvoiceMapper, DinasSaleInvoice> implements DinasSaleInvoiceService {

    @Autowired
    private DinasSaleInvoiceMapper saleInvoiceMapper;

    @Autowired
    private AppendixUtils appendixUtils;

    /**
     * @descriptin 添加销售发票
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<Integer> add(AuthPlatformUserInfo userInfo, SaleInvoiceRQ rq) {
        DinasSaleInvoice dinasSaleInvoice = BeanUtils.copyProperties(rq, DinasSaleInvoice.class);
        dinasSaleInvoice.setCompanyId(userInfo.getOrgId())
                .setCompanyName(userInfo.getOrg_name())
                .setCreateTime(new Date())
                .setCreateUser(userInfo.getId())
                .setUrl(appendixUtils.getJsonStr(rq.getUrlList()));
        if (!this.insert(dinasSaleInvoice)) {
            log.error("保存销售发票失败,调用{}类{}方法出错","DinasSaleInvoiceServiceImpl","add()");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dinasSaleInvoice.getId());
    }

    /**
     * @descriptin 编辑销售发票
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, SaleInvoiceRQ rq) {
        if (!ObjectUtils.isEmpty(rq.getId())) {
            DinasSaleInvoice dinasSaleInvoice = this.selectOne(new EntityWrapper<>(new DinasSaleInvoice()
                    .setId(rq.getId()).setDeleted(Status.FALSE.getKey())));
            if (ObjectUtils.isEmpty(dinasSaleInvoice)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
            }
            DinasSaleInvoice saleInvoice = BeanUtils.copyProperties(rq, DinasSaleInvoice.class);
            saleInvoice.setUpdateTime(new Date()).setUpdateUser(userInfo.getId())
                    .setUrl(appendixUtils.getJsonStr(rq.getUrlList()));
            if (!this.updateById(saleInvoice)) {
                log.error("编辑销售发票失败,调用{}类{}方法出错","DinasSaleInvoiceServiceImpl","update()");
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getId());
    }

    /**
     * @descriptin 查询销售发票详情
     * @author xin.huang
     * @param id
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<SaleInvoiceDTO> findInfo(Integer id) {
        SaleInvoiceDTO saleInvoice = saleInvoiceMapper.findInfo(id);
        if (Objects.nonNull(saleInvoice) && StringUtils.isNotBlank(saleInvoice.getUrl())) {
            saleInvoice.setUrlList(JSONObject.parseArray(saleInvoice.getUrl(), DinasUrlDTO.class));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleInvoice);
    }

    /**
     * @descriptin 批量删除销售发票
     * @author xin.huang
     * @param userInfo
     * @param ids
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<List<Integer>> batchDelete(AuthPlatformUserInfo userInfo, SaleBatchDeleteRQ rq) {
        DinasSaleInvoice dinasSaleInvoice = new DinasSaleInvoice()
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        this.update(dinasSaleInvoice, new EntityWrapper<DinasSaleInvoice>().in("id", rq.getIds()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getIds());
    }

    /**
     * @descriptin 查询销售发票列表
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @param pagination
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<List<SaleInvoiceDTO>> findList(AuthPlatformUserInfo userInfo, SaleInvoiceListRQ rq, Pagination pagination) {
        if (Objects.isNull(rq)) {
            rq = new SaleInvoiceListRQ();
        }
        rq.setCompanyId(userInfo.getOrgId());
        List<SaleInvoiceDTO> salePaymentList = saleInvoiceMapper.findList(rq, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, salePaymentList, PageUtils.transToPage(pagination));
    }
}
