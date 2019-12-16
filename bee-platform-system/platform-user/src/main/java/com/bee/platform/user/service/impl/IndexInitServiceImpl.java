package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.business.dto.IndexInitDTO;
import com.bee.platform.business.rq.IndexInitRQ;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.dao.mapper.IndexInitMapper;
import com.bee.platform.user.entity.IndexInit;
import com.bee.platform.user.service.IndexInitService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-03-20
 */
@Service
public class IndexInitServiceImpl extends ServiceImpl<IndexInitMapper, IndexInit> implements IndexInitService {

	@Autowired
	private IndexInitMapper indexInitMapper;
    /**
     * @notes  首页临时数据查询
     * @Author junyang.li
     * @Date 10:02 2019/3/20
     **/
    @Override
    public ResponseResult<IndexInitDTO> findAllNumbers() {
        List<IndexInit> list=baseMapper.selectList(new EntityWrapper<>());
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new IndexInitDTO());
        }
        IndexInitDTO dto= BeanUtils.copyProperties(list.get(0),IndexInitDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }

    /**
     * 修改临时数据
     */
	@Override
	public ResponseResult modifyAllNumbers(IndexInitRQ rq) {
        List<IndexInit> list=baseMapper.selectList(new EntityWrapper<>());
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        IndexInit init = BeanUtils.copyProperties(rq, IndexInit.class);
        indexInitMapper.update(init, new EntityWrapper<>());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
	}
}
