package com.bee.platform.user.authority.service.impl;

import cn.hutool.poi.excel.ExcelUtil;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dao.mapper.AuthInterfaceMapper;
import com.bee.platform.user.authority.dto.AuthInterfaceDto;
import com.bee.platform.user.authority.entity.AuthInterface;
import com.bee.platform.user.authority.rq.AuthInterfaceRQ;
import com.bee.platform.user.authority.rq.AuthInterfaceSelectRQ;
import com.bee.platform.user.authority.service.AuthInterfaceService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthInterfaceServiceImpl extends ServiceImpl<AuthInterfaceMapper, AuthInterface> implements AuthInterfaceService {

    @Autowired
    private AuthInterfaceMapper authInterfaceMapper;

    /**
     * @Description 条件查询接口列表
     * @Param authInterfaceRQs
     * @Date 2019/5/20 16:13
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<List<AuthInterfaceDto>> getList(AuthInterfaceSelectRQ authInterfaceSelectRQ, Page page) {
        Pagination pagination= PageUtils.transFromPage(page);
        EntityWrapper<AuthInterface> wrapper = new EntityWrapper<>();
        if (!StringUtils.isBlank(authInterfaceSelectRQ.getName())) {
            wrapper.like("name", authInterfaceSelectRQ.getName());
        }
        if (Objects.nonNull(authInterfaceSelectRQ.getBeeRouter())) {
            wrapper.eq("bee_router", authInterfaceSelectRQ.getBeeRouter());
        }
        if (Objects.nonNull(authInterfaceSelectRQ.getSubSys())) {
            wrapper.eq("sub_sys", authInterfaceSelectRQ.getSubSys());
        }
        if (Objects.nonNull(authInterfaceSelectRQ.getCreateTime())) {
            wrapper.eq("create_time", authInterfaceSelectRQ.getCreateTime());
        }
        wrapper.in("status", Status.TRUE.getKey().toString());
        wrapper.in("deleted", Status.FALSE.getKey().toString());
        List<AuthInterface> authInterfaces = authInterfaceMapper.selectPage(pagination, wrapper);
        List<AuthInterfaceDto> result = BeanUtils.assemble(AuthInterfaceDto.class, authInterfaces);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result, PageUtils.transToPage(pagination));
    }

    /**
     * @Description 添加接口
     * @Param authInterfaceRQs
     * @Date 2019/5/20 14:38
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> add(AuthInterfaceRQ authInterfaceRQs) {
        AuthInterface authInterface = BeanUtils.copyProperties(authInterfaceRQs, AuthInterface.class)
                .setCreateTime(new Date()).setStatus(Status.TRUE.getKey()).setUpdateTime(new Date());
        if (authInterfaceMapper.insert(authInterface) < 0) {
            log.error("接口添加失败，调用方法{}", "AuthInterfaceServiceImpl.add()");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 更新接口
     * @Param id
     * @Param authInterfaceRQs
     * @Date 2019/5/20 15:01
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> update(Integer id, AuthInterfaceRQ authInterfaceRQ) {
        AuthInterface authInterface = authInterfaceMapper.selectById(id);
        if (Objects.isNull(authInterface)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_NOT_EXIST);
        }
        AuthInterface newAuthInterface = BeanUtils.copyProperties(authInterfaceRQ,
                AuthInterface.class).setId(id).setUpdateTime(new Date());
        if (authInterfaceMapper.updateById(newAuthInterface) < 0) {
            log.error("接口更新失败，调用方法{}", "AuthInterfaceServiceImpl.update()");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 批量删除接口
     * @Param ids
     * @Date 2019/5/20 15:17
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> batchDelete(String ids) {
        String[] idArray = ids.split(",");
        if (idArray.length <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_ID_EMPTY);
        }
        List<Integer> idList = new ArrayList<>();
        for (String id : idArray) {
            idList.add(Integer.valueOf(id));
        }
        if (authInterfaceMapper.deleteBatchIds(idList) < 0) {
            log.error("接口删除失败，调用方法{}", "AuthInterfaceServiceImpl.batchDelete()");
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: excel批量导入接口
     * @Author: junyang.li
     * @Date: 14:42 2019/5/28
     * @param in :
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    public ResponseResult<ResCodeEnum> excelImport(InputStream in) {
        List<AuthInterface> list=ExcelUtil.getReader(in).read(0,1,AuthInterface.class);
        if(CollectionUtils.isEmpty(list)){
           return ResponseResult.buildResponseResult(ResCodeEnum.IMPORT_INTERFACE_FAIL);
        }
        authInterfaceMapper.insertAll(list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 根据子系统查询所有接口-后台
     * @param subSys
     * @return  List<AuthInterfaceDto>
     */
    @Override
    public List<AuthInterfaceDto> listInterfacesBySubSys(String subSys, String beeRouter) {
        Wrapper<AuthInterface> wrapper = new EntityWrapper<AuthInterface>()
                .eq("deleted",Status.FALSE.getKey());
        if (!StringUtils.isEmpty(subSys)){
            wrapper.and().eq("sub_sys",subSys);
        }
        if (!StringUtils.isEmpty(beeRouter)){
            wrapper.and().like("bee_router",beeRouter);
        }
        List<AuthInterface> interfaces = authInterfaceMapper.selectList(wrapper);

        return BeanUtils.assemble(AuthInterfaceDto.class,interfaces);
    }

    /**
     *
     * @param id
     * @return ResponseResult<AuthInterfaceDto>
     */
    @Override
    public ResponseResult<AuthInterfaceDto> getInterfaceDetail(String id) {
        AuthInterface ineterfaces = authInterfaceMapper.selectById(new AuthInterface()
                .setId(Integer.valueOf(id)).setDeleted(Status.FALSE.getKey()));
        AuthInterfaceDto detailDTO = BeanUtils.copyProperties(ineterfaces,AuthInterfaceDto.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,detailDTO);
    }

    /**
     * @Description 根据路由、URL、请求方式添加接口
     * @Param authInterfaceRQs
     * @Date 2019/06/08  12:17
     * @Author zhigang.zhou
     */
	@Override
	public ResponseResult<ResCodeEnum> addByRouterUrlMethod(AuthInterfaceRQ authInterfaceRQs) {
		String beeRouter = authInterfaceRQs.getBeeRouter();
		String url = authInterfaceRQs.getUrl();
		String type = authInterfaceRQs.getType();
		if (StringUtils.isBlank(beeRouter) || StringUtils.isBlank(url) || StringUtils.isBlank(type)) {
			log.info("beeRouter={}, url={}, type={}", beeRouter, url, type);
			return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
		}
		// 组装查询条件
		AuthInterface authInterface = new AuthInterface()
				.setBeeRouter(beeRouter)
		        .setUrl(url)
		        .setType(type);
		AuthInterface authInterfaceRes = authInterfaceMapper.selectOne(authInterface);
		// 有数据就跳过
		if(null != authInterfaceRes) {
			  log.info("exists, authInterfaceRes={}", authInterfaceRes);
			  return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_EXISTS);
		}
		// 没有数据就添加
		return add(authInterfaceRQs);
	}
}
