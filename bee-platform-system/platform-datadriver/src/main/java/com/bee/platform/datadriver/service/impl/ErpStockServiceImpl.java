package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.ErpStockMapper;
import com.bee.platform.datadriver.dto.ErpStockSearchListDTO;
import com.bee.platform.datadriver.entity.ErpStock;
import com.bee.platform.datadriver.rq.ErpStockSearchRQ;
import com.bee.platform.datadriver.service.ErpStockService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
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
        List<ErpStockSearchListDTO> dto = Lists.newArrayList();
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpStockServiceImpl", "searchStockByCondition");
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(ids)) {
            log.info("获取企业id为空，不进行查询");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }

        rq.setList(ids);
        dto = baseMapper.searchStockByCondition(rq, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }
}
