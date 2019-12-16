package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.dto.IndustryDTO;
import com.bee.platform.common.entity.IndustryInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.dao.mapper.IndustryMapper;
import com.bee.platform.user.entity.Industry;
import com.bee.platform.user.rq.IndustryRQ;
import com.bee.platform.user.service.IndustryService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.List;

/**
 * <p>
 * 行业分类信息表 服务实现类
 * </p>
 *
 * @author cheng.ke
 * @since 2019-04-24
 */
@Service
public class IndustryServiceImpl extends ServiceImpl<IndustryMapper, Industry> implements IndustryService {

    /**
     * 修改行业信息
     * @param rq 请求行业信息
     * @return 行业id
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer updateIndustry(IndustryRQ rq) {
        Industry industry = baseMapper.selectById(rq.getId());
        if(ObjectUtils.isEmpty(industry)){
            throw new BusinessException(ResCodeEnum.NO_DATA,ExceptionMessageEnum.ERROR_DATA);
        }
        industry.setIndustry(rq.getIndustry()).setPid(rq.getPid()).setLevel(rq.getLevel());
        if(baseMapper.updateById(industry)<=0){
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.INDUSTRY_SAVE_FAILED);
        }
        return industry.getId();
    }

    /**
     * 添加行业信息
     * @param rq 请求行业信息
     * @return 行业id
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer addIndustry(IndustryRQ rq) {
        Industry industry = new Industry().setPid(rq.getPid()).setIndustry(rq.getIndustry()).setLevel(rq.getLevel());
        if(baseMapper.insert(industry)<=0){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.INDUSTRY_SAVE_FAILED);
        }
        return industry.getId();
    }

    /**
     * 根据父级id查询行业列表
     *
     * @param pid 行业父级id
     * @return 行业列表
     */
    @Override
    public List<Industry> getIndustryByParentId(int pid) {

        return baseMapper.selectList(new EntityWrapper<Industry>().eq("pid", pid));
    }

    /**
     * 根据id查询行业详情
     *
     * @param id 行业id
     * @return 行业详情信息
     */
    @Override
    public IndustryInfo getIndustryById(int id) {

        return BeanUtils.copyProperties(baseMapper.selectById(id), IndustryInfo.class);
    }

    /**
     * 根据industryId返回IndustryDTO对象
     *
     * @param id 行业id
     * @return 行业信息对象
     */
    @Override
    public IndustryDTO selectIndustry(String id) {
        IndustryDTO dto = new IndustryDTO();
        if (StringUtils.isEmpty(id)) {
            return dto;
        }
        // 子级行业信息
        IndustryInfo childIndustry = getIndustryById(Integer.valueOf(id));
        if (!ObjectUtils.isEmpty(childIndustry.getPid())) {
            // 父级行业信息
            IndustryInfo parentIndustry = getIndustryById(childIndustry.getPid());
            return dto.setChildIndustry(childIndustry).setParentIndustry(parentIndustry);
        }
        return dto;
    }
}
