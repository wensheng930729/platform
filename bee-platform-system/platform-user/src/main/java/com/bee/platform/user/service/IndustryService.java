package com.bee.platform.user.service;

import com.bee.platform.common.dto.IndustryDTO;
import com.bee.platform.common.entity.IndustryInfo;
import com.bee.platform.user.entity.Industry;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.rq.IndustryRQ;

import java.util.List;

/**
 * <p>
 * 行业分类信息表 服务类
 * </p>
 *
 * @author cheng.ke
 * @since 2019-04-24
 */
public interface IndustryService extends IService<Industry> {
    /**
     * 根据父级id查询行业列表
     * @param pid 父级id
     * @return 行业列表
     */
    List<Industry> getIndustryByParentId(int pid);
    /**
     * 根据id查询行业详情
     * @param id 行业id
     * @return 行业详情信息
     */
    IndustryInfo getIndustryById(int id);
    /**
     * 根据industryId返回IndustryDTO对象
     * @param id 行业id
     * @return 行业信息对象
     */
    IndustryDTO selectIndustry(String id);

    /**
     * 添加行业信息
     * @param rq 请求行业信息
     * @return 添加的行业信息id
     */
    Integer addIndustry(IndustryRQ rq);

    /**
     * 修改行业信息
     * @param rq 请求行业信息
     * @return 行业id
     */
    Integer updateIndustry(IndustryRQ rq);
}
