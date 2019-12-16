package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.ErpStockMapper;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderSearchListDTO;
import com.bee.platform.datadriver.dto.ErpStockDetailDTO;
import com.bee.platform.datadriver.dto.ErpStockSearchListDTO;
import com.bee.platform.datadriver.entity.ErpStock;
import com.bee.platform.datadriver.rq.ErpStockDetailListRQ;
import com.bee.platform.datadriver.rq.ErpStockSearchRQ;
import com.bee.platform.datadriver.service.ErpStockService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 库存表 服务实现类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-03
 */
@Slf4j
@Service
public class ErpStockServiceImpl extends ServiceImpl<ErpStockMapper, ErpStock> implements ErpStockService {

    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    /**
     * 条件搜索现存明细
     *
     * @param
     * @param rq
     * @param page
     * @param
     * @return
     */
    @Override
    public ResponseResult<List<ErpStockSearchListDTO>> searchStockByCondition(ErpStockSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
        rq.setCompanyId(companyId);
        List<ErpStockSearchListDTO> dto = baseMapper.searchStockByCondition(rq, pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpStockSearchListDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 查询现存明细详情列表
     *
     * @param rq   请求参数
     * @param page 分页对象
     * @return 详情列表
     */
    @Override
    public ResponseResult<List<ErpStockDetailDTO>> getStockDetailByCondition(ErpStockDetailListRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);

        List<ErpStockDetailDTO> dto = baseMapper.getStockDetailByCondition(rq, pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpStockDetailDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }
}
